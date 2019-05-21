(ns lrn-utils.systemd
  "This notifies systemd (look up `WatchdogSec`) that our process is alive."
  (:require [clojure.core.async :as async]
            [lrn-utils.core :as core]
            [taoensso.timbre :as log])
  (:import [info.faljse.SDNotify SDNotify]))


(defonce ^:private -watchdog-future- (atom nil))


(defn stop-watchdog! []
  (locking -watchdog-future-
    (when-let [f @-watchdog-future-]
      (future-cancel f)
      (reset! -watchdog-future- nil))))


(defn start-watchdog! "Returns a future if the process is running under systemd -- or NIL otherwise.
  `interval`: millies."
  ([^long interval] (start-watchdog! interval false))

  ([^long interval debug?]
   (if-not (get (System/getenv) "NOTIFY_SOCKET")
     (log/info "systemd: NOTIFY_SOCKET env var not set; not starting watchdog.")
     (locking -watchdog-future-
       (log/info "systemd: watchdog starting.")
       (stop-watchdog!)
       (SDNotify/sendNotify) ;; "READY=1"
       (->> (future
              (loop []
                (try
                  (when debug? (log/info "systemd: calling SDNotify/sendWatchdog"))
                  (SDNotify/sendWatchdog)
                  (Thread/sleep interval)
                  (catch java.lang.InterruptedException ex
                    (log/info ex "systemd: watchdog is stopping.")
                    (throw ex))
                  (catch Throwable ex
                    (log/fatal ex "systemd: watchdog seems to be failing; this can't be good, but trying to recover.")
                    (Thread/sleep 3000)))
                (recur)))
            (reset! -watchdog-future-))))))
