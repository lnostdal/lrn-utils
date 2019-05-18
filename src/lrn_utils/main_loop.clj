(ns lrn-utils.main-loop
  (:require [clojure.core.async :as async]
            [taoensso.timbre :as log])
  (:use lrn-utils.core))



(defn start "The \"main loop\" of some app or closed-off process; the idea is that all higher-level non-local state is immutable and held in `state` as `handle-fn` processes chunks of events sent to `event-ch`. If `event-ch` is closed this loop ends and returns the final `state` value.
  `event-ch`: Vectors are sent to this; each vector holds a chunk of events for processing by `handle-fn`.
  `state`: Some initial state.
  `handle-fn`: (fn [state events] ..) => state"
  [id event-ch state handle-fn]
  (try
    (log/info "Starting main loop" id "for" event-ch)
    (loop [state state]
      (if-let [events (async/<!! event-ch)]
        (recur (try
                 (handle-fn state events)
                 (catch Throwable e
                   (log/fatal e "Top-level exception; partial update of `state` lost"
                              "and the latest chunk from `event-ch` was thrown away!"
                              {:id id, :event-ch event-ch, :events events})
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
