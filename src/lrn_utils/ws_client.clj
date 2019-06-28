(ns lrn-utils.ws-client
  "Goals, features etc.:
  * Use c.c.async as an API -- so switching to something else later is easier."
  (:require [clojure.core.async :as async]
            [taoensso.timbre :as log]
            [gniazdo.core :as ws]
            [jsonista.core :as json])
  (:use lrn-utils.core))


;; TODO: Consider using Jetty directly for client -- and perhaps later also for server? This is a bit messy as is now, but should(TM) be stable. Portions of this has been running fine for years in context of my trading systems.


(defonce -connections- (atom {})) ;; endpoint-id -> conn-ctx


;; TODO: I don't think this thing should do this much "stuff"? See the stop method here: https://www.eclipse.org/jetty/javadoc/current/org/eclipse/jetty/websocket/client/WebSocketClient.html (`iconn` variable below).
(defn close! [endpoint-id]
  (swap! -connections-
         (fn [connections]
           (if-let [c (connections endpoint-id)]
             (do
               #_(dbgc 'close! c)
               (vreset! (:abort? c) true)
               #_(when (realized? (:conn c))
                   (ws/close @(:conn c)))
               (.stop ^org.eclipse.jetty.websocket.client.WebSocketClient (:iconn c))
               (async/close! (:ochan c))
               (async/close! (:ichan c))
               (async/untap-all (:michan c))
               (dissoc connections endpoint-id))
             connections))))



;; TODO: Switch from dbg-println to log/*.
;; TODO: I'm pretty sure I can get rid of like 70% of this code if I used Jetty directly instead of bothering with gniazdo. The code should be OK'ish tho; it has been running solid for years in my trading system. :)
;; TODO: For compression, I should look in what https://github.com/ptaoussanis/nippy is doing i.e. lz4 compression!
;; TODO: I don't think the initial connection attempt should block.
(defn connect! "Returns NIL if initial connection attempt (blocking) was aborted -- or `endpoint-id` otherwise. Messages sent to the :ichan channel are either instances of String or a vector representing a binary array: [data offset len].
  `endpoint-id`: A String or NIL (in which case a UUID will be generated).
  `url`: URL for WSS connection.
  `:max-text-message-buffer-size`: Default is 10000000.
  `:max-text-message-size`: Default is 10000000.
  `:idle-timeout`: Default is 120 (seconds).
  `:retry-pause`: Default is 1 (seconds)."
  ([endpoint-id ^String url] (connect! endpoint-id url {}))
  ([endpoint-id ^String url m]
   (when endpoint-id (close! endpoint-id))
   (let [endpoint-id (or endpoint-id (uuid))
         bnds (get-thread-bindings), abort? (volatile! false), retry-pause (double (or (:retry-pause m) 0.5))
         conn (promise)
         iconn (with1 (ws/client (java.net.URI/create url)) ;; "Internal connection".
                 (.setMaxTextMessageBufferSize it (or (:max-text-message-buffer-size m) 10000000)))
         ichan (or (:ichan m) ;; Retry connection after exception.
                   (:ichan (get @-connections- endpoint-id)) ;; When redefining using the same `endpoint-id`.
                   (async/chan 1))
         michan (or (with1 (:michan m) ;; Retry connection after exception.
                      (when it (assert (:ichan m))))
                    (:michan (get @-connections- endpoint-id)) ;; When redefining using the same `endpoint-id`.
                    (async/mult ichan))
         ochan (or (:ochan m) ;; Retry connection after exception.
                   (:ochan (get @-connections- endpoint-id))
                   (async/chan 1))]
     (with (.getPolicy iconn)
       (.setMaxTextMessageSize it (or (:max-text-message-size m) 10000000))
       (.setIdleTimeout it (* 1000 (long (or (:idle-timeout m) 120)))))
     (.start iconn)
     (swap! -connections- assoc endpoint-id {:endpoint-id endpoint-id, :abort? abort?, :iconn iconn
                                             :ichan ichan, :michan michan, :ochan ochan})
     (while (and (not (realized? conn)) (not @abort?) (not (.isInterrupted (Thread/currentThread))))
       (try
         (deliver conn
                  (ws/connect url
                    :client iconn
                    :headers {"x-lrn-endpoint-id" endpoint-id}
                    ;;:extensions ["permessage-deflate"] ;; TODO: Doesn't actually seem to work! https://github.com/stalefruits/gniazdo/blob/master/test/gniazdo/core_test.clj#L141
                    :on-connect
                    (fn [^org.eclipse.jetty.websocket.api.Session c]
                      #_(log/info :on-connect)
                      #_(with-bindings bnds
                          #_(dbg-println "[DEF-MARKET-STREAMER, :ON-CONNECT]:" c)
                          (try
                            (def -blah- (.. c getUpgradeRequest getExtensions))
                            (catch Throwable e
                              (pprint e)
                              (throw e)))))

                    :on-receive
                    (fn [s]
                      (with-bindings bnds
                        (when-not @abort?
                          (when-not (async/>!! ichan s)
                            (log/warn "Message thrown away; input channel closed early?")
                            (close! endpoint-id)))))

                    :on-binary
                    (fn [data offset len]
                      (with-bindings bnds
                        (when-not @abort?
                          (when-not (async/>!! ichan [data offset len])
                            (close! endpoint-id)))))

                    :on-error
                    (fn [^Throwable e]
                      (with-bindings bnds
                        (when-not @abort?
                          (dbg-println "[CONNECT!, :ON-ERROR]:" e))))

                    :on-close
                    (fn [^long status-code ^String reason]
                      (with-bindings bnds
                        (when-not @abort?
                          (dbg-println "[CONNECT!, :ON-CLOSE]:" status-code reason)
                          (Thread/sleep (* retry-pause 1000))
                          (try ;; Reconnect and recycle existing params.
                            (connect! endpoint-id url (assoc m :ichan ichan :michan michan, :ochan ochan))
                            (catch Throwable e
                              (log/fatal "Could not reconnect; connection permanently lost"
                                         {:endpoint-id endpoint-id} e))))))))
         (catch InterruptedException e
           (close! endpoint-id)
           (throw e))
         (catch Throwable e
           (dbg-println "[CONNECT!," endpoint-id "]:" e)))
       (when-not @abort?
         (Thread/sleep (* retry-pause 1000))))
     (when-not @abort?
       (async/go-loop []
         (if-let [msg (async/<! ochan)]
           (do
             (ws/send-msg @conn msg)
             (recur))
           (close! endpoint-id)))
       endpoint-id))))



(defn get-ochan [endpoint-id]
  "For writing stuff to `endpoint-id`. Returns: an async/chan for writing."
  (:ochan (get @-connections- endpoint-id)))



(defn get-michan [endpoint-id]
  "For reading stuff from `endpoint-id`. Returns: an async/mult for reading (via async/tap)."
  (:michan (get @-connections- endpoint-id)))
