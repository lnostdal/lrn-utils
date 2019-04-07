(ns lrn-utils.logging
  "Two parts: A HTTP client for sending log events (wrapped in an optional timbre appender) and a HTTP server for receiving them.
  Does not guarantee delivery and does not check for double-delivery; other tools should do this."
  (:require [org.httpkit.server :as http-server]
            [org.httpkit.client :as http-client]
            [taoensso.encore :as enc]
            [taoensso.timbre :as timbre]
            [jsonista.core :as json]
            [clojure.core.async :as async])
  (:use lrn-utils.core))

;; FIXME: While this is not a goal of this component, do some *basic* delivery and double-delivery checks?
;; FIXME: Send chunks and use the ASYNC-CONSUME-BUFFER Fn!


(defonce ^:private -server- (atom nil)) ;; Actual server object returned by HTTP-KIT.
(defonce -http-timeout- (atom 10000)) ;; millis



(defn server-handler-println [req]
  ;; This is just a dummy handler. Usually you'd want to:
  ;;   * Put this server behind a server that does HTTPS/TLS.
  ;;   * Put an API-key or similar in the request (e.g. URL) and use this for filtering.
  (dbg-println req)
  (dbg-println (:raw (json/read-value (slurp (.bytes (:body req))))))
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
                                :body (-> {:raw (:data m), :event-id event-id, :api-key (:api-key m)}
                                          (json/write-value-as-string))})]
    (when (not= 201 (:status res))
      (println "## lrn-utils.logger/send-event =>")
      (clojure.pprint/pprint res))))



(def ^:private -http-appender-ch-
  (with1 (async/chan (async/sliding-buffer 3))
    (async/go-loop []
      (try
        (when-let [event (async/<! it)]
          (send-event event))
        (catch Throwable e
          (println "[lrn-utils.logger/-http-appender-ch-]:" e)
          (Thread/sleep 1000)))
      (recur))))



(defn http-appender "Appender for timbre.
  :url: URL of http server."
  [m]
  {:enabled? true
   :async? false
   :min-level (or (:min-level m) :warn)
   :rate-limit nil
   :output-fn :inherit
   :fn
   (fn [data]
     (let [{:keys [output_]} data
           output-str (force output_)]
       (async/put! -http-appender-ch- {:data output-str, :api-key (:api-key m), :url (:url m)})))})



#_(server-start 6243 #'server-handler-println)

#_(timbre/merge-config!
   {:appenders {::appender (#'http-appender {:url "http://localhost:6243"})}})

