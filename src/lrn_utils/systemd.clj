(ns lrn-utils.systemd
  "This notifies systemd (look up `WatchdogSec`) that our process is alive."
  (:require [clojure.core.async :as async]
            [lrn-utils.core :as core])
  (:import [info.faljse.SDNotify SDNotify]))


(def ^:private -dummy-ch- (async/chan))


(defn start-watchdog! "`interval` millies."
  [^long interval]
  (when (get (System/getenv) "NOTIFY_SOCKET")
    (async/go-loop []
      (let [timeout-ch (async/timeout interval)]
        (async/alt!
          -dummy-ch- (throw (ex-info "This should never happen.." {}))
          timeout-ch (SDNotify/sendWatchdog)))
      (recur))))
