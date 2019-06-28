(ns lrn-utils.ws-server
  "Goals, features etc.:
  * Use c.c.async as an API -- so switching to something else later is easier."
  (:require [clojure.core.async :as async]
            [taoensso.timbre :as log]
            [org.httpkit.server :as server]
            [jsonista.core :as json])
  (:use lrn-utils.core))


(defonce -connections- (atom {})) ;; endpoint-id -> conn-ctx


(defn- on-receive [conn-ctx ws-ch message]
  (when-not (async/>!! (:ichan conn-ctx) message)
    (log/warn (ex-info "Got message, but input channel was closed.."
                       {:conn-ctx conn-ctx, :ws-ch ws-ch, :message message}))))



(defn- on-close [conn-ctx on-close-fn ws-ch status]
  (log/info "on-close" {:conn-ctx conn-ctx, :ws-ch ws-ch, :status status})
  (on-close-fn conn-ctx ws-ch status)
  (swap! -connections- dissoc (:endpoint-id conn-ctx))
  (async/close! (:ochan conn-ctx))
  (async/close! (:ichan conn-ctx))
  (async/untap-all (:michan conn-ctx)))



(defn- on-open [conn-ctx on-open-fn ws-ch]
  (log/info "on-open" {:conn-ctx conn-ctx, :ws-ch ws-ch})
  (swap! -connections- assoc (:endpoint-id conn-ctx) conn-ctx)
  (on-open-fn conn-ctx ws-ch)
  (async/go-loop []
    (if-let [msg (async/<! (:ochan conn-ctx))]
      (do
        (server/send! ws-ch msg)
        (recur))
      (server/close ws-ch)))) ;; Should trigger a call to ON-CLOSE which will clean things up.. TODO: Confirm this.



(defn- ws-handler [on-open-fn on-close-fn server-endpoint-id req]
  (if (:websocket? req)
    (let [client-endpoint-id (get (:headers req) "x-lrn-endpoint-id")
          conn-ctx (or (get @-connections- client-endpoint-id) ;; Use existing c.c.async channels during reconnects.
                       (let [ichan (async/chan 1), michan (async/mult ichan), ochan (async/chan 1)]
                         {:endpoint-id client-endpoint-id, :ochan ochan, :ichan ichan, :michan michan}))]
      (server/as-channel req {:on-receive (partial #'on-receive conn-ctx)
                              :on-open (partial #'on-open conn-ctx on-open-fn)
                              :on-close (partial #'on-close conn-ctx on-close-fn)}))
    {:status 200, :body "ws-server/ws-handler: hello world"}))



;; TODO: Get rid of this? Let user deal with the server instance(s) etc..
(defonce -server- (atom nil))

(defn stop! []
  (locking -server-
    (when-let [server @-server-]
      (server :timeout 1000)
      (reset! -server- nil))))

(defn start! "`on-open-fn`: (fn [conn-ctx ws-ch] ..)
  `on-close-fn`: (fn [conn-ctx ws-ch status] ..)"
  [server-endpoint-id port on-open-fn on-close-fn]
  (locking -server-
    (stop!)
    (reset! -server- (server/run-server (partial #'ws-handler on-open-fn on-close-fn server-endpoint-id)
                                        {:port port}))))



(defn get-ochan [endpoint-id]
  "For writing stuff to `endpoint-id`. Returns: an async/chan for writing."
  (:ochan (get @-connections- endpoint-id)))



(defn get-michan [endpoint-id]
  "For reading stuff from `endpoint-id`. Returns: an async/mult for reading (via async/tap)."
  (:michan (get @-connections- endpoint-id)))
