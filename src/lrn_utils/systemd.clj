(ns lrn-utils.systemd
  "This notifies systemd (look up `WatchdogSec`) that our process is alive."
  (:require [clojure.core.async :as async]
            [lrn-utils.core :as core]
            [taoensso.timbre :as log])
  (:import [info.faljse.SDNotify SDNotify]))


(def ^:private -dummy-ch- (async/chan))


(defn start-watchdog! "`interval` millies."
  [^long interval]
  (when (get (System/getenv) "NOTIFY_SOCKET")
    (log/info "systemd watchdog started.")
    (async/go-loop []
      (try
        ;; TODO/FIXME: This worker pool might be fixed actually; I think it is -- so this might not be a great idea.
        (let [timeout-ch (async/timeout interval)]
          (async/alt!
            -dummy-ch- (throw (ex-info "This should never happen.." {}))
            timeout-ch (SDNotify/sendWatchdog)))
        (catch Throwable ex
          (log/fatal ex "systemd watchdog seems to be failing; this can't be good.")
          (Thread/sleep 3000)))
      (recur))))
