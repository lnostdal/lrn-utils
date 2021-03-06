(ns lrn-utils.http
  (:require [org.httpkit.server :as http-server]
            [org.httpkit.client :as http-client]
            [jsonista.core :as json])
  (:use lrn-utils.core)
  (:import java.net.SocketTimeoutException))



(defonce -server- (atom nil)) ;; Actual server object returned by HTTP-KIT.
(defonce -http-timeout- (atom 10000)) ;; millis



(defn server-stop []
  (locking -server-
    (when-let [server @-server-]
      (server :timeout 1000)
      (reset! -server- nil))))



(defn server-start "`handler-fn`: (fn [req] {:status 200, :body (json/write-value-as-bytes {..})})"
  [port handler-fn]
  (locking -server-
    (server-stop)
    (reset! -server- (http-server/run-server handler-fn {:port port}))))



(defonce -client-timeout- (atom 10000)) ;; millis



(defn- json-req-deref-res
  [res-prom opts ctx]
  (let [res (deref res-prom (:timeout opts) ::timeout)]
    (if (= res ::timeout)
      (throw (with (SocketTimeoutException. "Internal timeout while waiting for response from HTTP-KIT.")
               (ex-info (str it) {:ctx ctx, :opts opts} it)))
      (if-let [e (:error res)]
        (throw (ex-info (str e) {:res res, :ctx ctx, :opts opts} e))
        (try (update res :body #(json/read-value % json-keywordize))
             (catch Throwable e
               (throw (ex-info (str e) {:res res, :ctx ctx, :opts opts} e))))))))



(defn json-req "Simple JSON request + JSON response handling. Blocks and returns a parsed JSON response."
  ([url data] (json-req url data {}))
  ([url data opts]
   (let [req (merge {:method :post, :url url, :content-type :json, :throw-exceptions false
                     :body (json/write-value-as-bytes data)
                     :socket-timeout @-client-timeout-, :connection-timeout @-client-timeout-}
                    opts)]
     (as-> (try @(http-client/request req)
                (catch Throwable e
                  (throw (ex-info "Exception during HTTP request" {:request req} e)))) res
       (-> (update res :body #(try (json/read-value % json-keywordize)
                                   (catch Throwable e
                                     (throw (ex-info "Exception during JSON parsing" {:request req, :response res} e)))))
           (assoc :request req))))))
