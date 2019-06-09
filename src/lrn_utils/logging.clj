(ns lrn-utils.logging
  "Two parts: A HTTP client for sending log events (wrapped in an optional timbre appender) and a HTTP server for receiving them.

  Current goals, features and somewhat missing features:

    * Mail alerts are non-blocking via a sliding buffer – which means that this will not slow down the rest of your system(s) even if it starts generating a lot of exceptions or messages.
    * What does this mean? Once you get an email you must check the *real* logs.
    * Currently does not guarantee delivery and does not check for double-delivery; other tools should do this."
  (:require [org.httpkit.server :as http-server]
            [org.httpkit.client :as http-client]
            [taoensso.encore :as enc]
            [taoensso.timbre :as timbre]
            [jsonista.core :as json]
            [clojure.core.async :as async]
            [lrn-utils.db :as db]
            [lrn-utils.db-queue :as db-queue])
  (:use lrn-utils.core))

;; FIXME: While this is not a goal of this component, do some *basic* delivery and double-delivery checks?
;; FIXME: Send chunks and use the ASYNC-CONSUME-BUFFER Fn!


(defonce ^:private -server- (atom nil)) ;; Actual server object returned by HTTP-KIT.
(defonce -http-timeout- (atom 10000)) ;; millis



(defn- server-handler-println [req]
  ;; This is just a dummy handler. Usually you'd want to:
  ;;   * Put this server behind a server that does HTTPS/TLS.
  ;;   * Put an API-key or similar in the request (e.g. URL) and use this for filtering.
  (dbgc 'lrn-utils.logging/server-handle-println (json/read-value (:body req) json-keywordize))
  {:status 201, :body "OK"})



(defn server-stop []
  (locking -server-
    (when-let [server @-server-]
      (server :timeout 1000)
      (reset! -server- nil))))



(defn server-start [port req-handler]
  (locking -server-
    (server-stop)
    (reset! -server- (http-server/run-server req-handler {:port port}))))



(defn- send-event [m]
  (let [event-id (.toString (java.util.UUID/randomUUID))
        res @(http-client/post (:url m)
                               {:timeout @-http-timeout-
                                :body (-> {:api-key (:api-key m), :data (assoc (:data m) :event-id event-id)}
                                          (json/write-value-as-string))})]
    (when (not= 201 (:status res))
      (println "## lrn-utils.logger/send-event =>")
      (clojure.pprint/pprint res))))



(def ^:private -email-appender-ch-
  (with1 (async/chan (async/sliding-buffer 3))
    (async/go-loop []
      (try
        (when-let [event (async/<! it)]
          (send-event event))
        (catch Throwable e
          (println "[lrn-utils.logger/-email-appender-ch-]:" e)
          (Thread/sleep 1000)))
      (recur))))



(def ^:private -db-appender-ch-
  (with1 (async/chan (async/sliding-buffer 3))
    (async/go-loop []
      (try
        (when-let [event (async/<! it)]
          )
        (catch Throwable e
          (println "[lrn-utils.logger/-db-appender-ch-]:" e)
          (Thread/sleep 1000)))
      (recur))))



(defn email-appender "Appender for timbre.
  :url: URL of HTTP server."
  [m]
  {:enabled? true
   :async? false ;; We use a non-blocking buffer anyway (-email-appender-ch-).
   :min-level (or (:min-level m) :warn)
   :rate-limit nil
   :output-fn (partial timbre/default-output-fn {:stacktrace-fonts {}})
   :fn (fn [data]
         ;; TODO: Have one map (`m`) for the timbre appender and another for the actual log event.
         (async/put! -email-appender-ch- {:api-key (:api-key m), :url (:url m)
                                          :data {:raw (force (:output_ data)), :hostname (force (:hostname_ data))
                                                 :timestamp (force (:timestamp_ data))}}))})



(defn db-appender "Appender for timbre.
  :url: URL of HTTP server."
  [m]
  {:enabled? true
   :async? false ;; We use a non-blocking buffer anyway (-http-appender-ch-).
   :min-level (or (:min-level m) :warn)
   :rate-limit nil
   :output-fn (partial timbre/default-output-fn {:stacktrace-fonts {}})
   :fn
   (fn [data]
     (let [{:keys [output_]} data
           output-str (force output_)]
       ;; TODO: Have one map (`m`) for the timbre appender and another for the actual log event.
       ;; TODO: It'd be super cool if we could also log real objects here instead of only strings. Tho I think this should happen in the -HTTP-APPENDER-CH- go-block.
       (async/put! -db-appender-ch- {:data output-str})))})



(defn- do-test []
  (server-start 6243 #'server-handler-println)
  (timbre/merge-config!
   {:appenders {::appender (#'email-appender {:url "http://localhost:6243"})}})
  (timbre/fatal (Exception. "hello"))
  (Thread/sleep 500)
  (server-stop)
  nil)
