(ns lrn-utils.main-loop
  "  * Deals with cases where you only want a single response to a bulk of events."
  (:require [clojure.core.async :as async]
            [taoensso.timbre :as log])
  (:use lrn-utils.core))



(def ^:dynamic *iteration-ctx*
  "Some active context during processing of a chunk of events. Bound to an atom holding a map in context of main-loop.")


(defn run-once-only "Run `f` denoted as `k` once only in context of a single main-loop iteration.
  `f`: (fn [state] ..) => state"
  [k f]
  (swap! *iteration-ctx* update ::once-only assoc k f))


(defn- do-once-only [state]
  (reduce #((val %2) %1) state (::once-only @*iteration-ctx*)))



(defn start "The \"main loop\" of some app or closed-off process; the idea is that all higher-level non-local state is immutable and held in `state` as `handle-fn` processes chunks of events sent to `event-ch`. If `event-ch` is closed this loop ends and returns the final `state` value.
  `event-ch`: Vectors are sent to this; each vector holds a chunk of events for processing by `handle-fn`.
  `state`: Some initial state.
  `handle-fn`: (fn [state events] ..) => state"
  [id event-ch state handle-fn]
  (try
    (log/info "Starting main loop" id "for" event-ch)
    (loop [state state]
      (if-let [event-chunks (async-consume-buffer event-ch true)]
        (recur (try
                 (binding [*iteration-ctx* (atom {})]
                   (let [new-state (reduce #(handle-fn %1 %2) state event-chunks)]
                     (do-once-only new-state)))
                 (catch Throwable e
                   ;; FIXME: Would it not make sense to also catch an exception that _intended_ for state update to be thrown away? We'll need to deal correctly with consumers here.
                   ;; FIXME: How do we deal with consumers that have already been updated about state change that will be reverted here at this point? They will all need to be re-synced? Need some re-sync callback.
                   (log/fatal e "Top-level exception; partial update of `state` lost"
                              "and the latest chunk from `event-ch` was thrown away!"
                              {:id id, :event-ch event-ch, :event-chunks event-chunks})
                   (Thread/sleep 1000) ;; TODO: Better rate-limiting? Top-level ex shouldn't happen normally tho.
                   state)))
        state))
    (finally
      (log/info "Ending main loop" id "for" event-ch))))



;; TODO: Move to some test.
#_(let [event-ch (async/chan 10)]
    (async/put! event-ch [1 2 3])
    (async/put! event-ch [4 5 6])
    (async/close! event-ch)
    (assert (= 21 (main-loop :test-loop event-ch 0 #(reduce + %1 %2)))))
