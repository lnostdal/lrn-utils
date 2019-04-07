(ns lrn-utils.logging
  "Two parts: A HTTP client for sending log events (wrapped in an optional timbre appender) and a HTTP server for receiving them.
  Does not guarantee delivery and does not check for double-delivery; other tools should do this."
  (:require [org.httpkit.server :as http-server]
            [org.httpkit.client :as http-client]
            [taoensso.encore :as enc]
            [taoensso.timbre :as timbre]
            [jsonista.core :as json])
  (:use lrn-utils.core))



(defonce ^:private -server- (atom nil)) ;; Actual server object returned by HTTP-KIT.



;; FIXME: While this is not a goal of this component, do some *basic* double-delivery checks via some sort of middleware?
(defn server-handler-println [req]
  ;; This is just a dummy handler. Usually you'd want to:
  ;;   * Put the server behind a server that does HTTPS/TLS.
  ;;   * Put an API-key or similar in the request (e.g. URL) and use this for filtering.
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



(defn http-appender "Appender for timbre.
  :url: URL of http server."
  [m]
  {:enabled? true
   :async? false
   :min-level :warn
   :rate-limit nil
   :output-fn :inherit
   :fn
   (fn [data]
     (let [{:keys [output_]} data
           output-str (force output_)
           event-id (.toString (java.util.UUID/randomUUID))]
       ;; FIXME: While this is not a goal of this component, do some *basic* delivery checks here?
       (http-client/post (:url m)
                         {:body (-> {:raw output-str, :event-id event-id}
                                    (json/write-value-as-string))})))})



#_(timbre/merge-config!
   {:appenders {::appender (#'http-appender {:url "http://localhost:8080"})}})
