(ns lrn-utils.test-ws
  (:require [clojure.core.async :as async]
            [taoensso.timbre :as log]
            [jsonista.core :as json]
            [lrn-utils.ws-client :as ws-client]
            [lrn-utils.ws-server :as ws-server])
  (:use lrn-utils.core))



(defn do-test []
  (let [server (ws-server/start! "server-id" 9999
                                 (fn [conn-ctx ws-ch] #_(dbg-println "server-start!, on-open"))
                                 (fn [conn-ctx ws-ch status] #_(dbg-println "server-start!, on-close:" status)))
        endpoint-id "test"]
    (try
      (ws-client/connect! endpoint-id "ws://127.0.0.1:9999")

      ;; Print incoming messages on server and client!
      (let [ich (with1 (async/chan 5) (async/tap (ws-server/get-michan endpoint-id) it))]
        (async/go-loop []
          (when-let [msg (async/<! ich)]
            (dbg-println "server got a message from client:" msg)
            (recur))))
      (let [ich (with1 (async/chan 5) (async/tap (ws-client/get-michan endpoint-id) it))]
        (async/go-loop []
          (when-let [msg (async/<! ich)]
            (dbg-println "client got a message from server:" msg)
            (recur))))

      ;; Send some messages both ways!
      (dotimes [i 10]
        (async/>!! (ws-client/get-ochan endpoint-id) (str "msg nr. " i " from client")))
      (dotimes [i 10]
        (async/>!! (ws-server/get-ochan endpoint-id) (str "msg nr. " i " from server")))

      (Thread/sleep 2000)
      (catch Throwable e
        (dbg-println e))
      (finally
        (ws-client/close! endpoint-id)
        (ws-server/stop!)))))
