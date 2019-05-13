(ns lrn-utils.db
  (:refer-clojure :exclude [io!])
  (:require [taoensso.timbre :as log]
            [clojure.core.async :as async]
            [clj-uuid :as uuid]
            [environ.core :refer [env]]
            [clojure.java.jdbc :as jdbc]
            [postgre-types.json :as pg-types]
            [jsonista.core :as json])

  (:use [lrn-utils.core :as lrn])

  (:import [clojure.lang ExceptionInfo])
  (:import [java.sql SQLException])
  (:import [java.util.concurrent TimeoutException])
  (:import [com.impossibl.postgres.jdbc PGDataSource]
           [com.impossibl.postgres.api.jdbc PGNotificationListener]))



(let [om (json/object-mapper {:decode-key-fn lrn/keywordize})]
  (pg-types/add-jsonb-type json/write-value-as-string #(json/read-value % om)))



(defn identifier [x]
  (jdbc/as-sql-name (jdbc/quoted :ansi) x))



(def -db-conn-
  (delay
   {:datasource
    (com.zaxxer.hikari.HikariDataSource.
     (doto (com.zaxxer.hikari.HikariConfig.)
       (.setJdbcUrl (or (env :database-url)
                        "jdbc:pgsql://127.0.0.1:5432/test?user=test&password=test"))
       (.setMaximumPoolSize (* 2 (. (Runtime/getRuntime) availableProcessors)))
       (.setAutoCommit true) ;; FIXME: Not sure about this; nice in the REPL during dev perhaps?
       (.setLeakDetectionThreshold 5)))}))



;; FIXME: It'd be great if we merged these into one dyn var.
(def ^:dynamic *db*)
(def ^:dynamic *run-post-commit*)
(def ^:dynamic *run-post-rollback*)
(def ^:dynamic *in-post-tx-handling?* false)

(defonce -tx-id->input-ch- (atom {})) ;; TODO: Use the mount lib so we can use the :stop handler later.

(def ^:const -checks- true) ;; Extra asserts etc..
(def ^:const -debug- true) ;; Generate extra logging output etc.



;; For SQLException/getSQLState and/or SQLException/getErrorCode:
;;   https://www.postgresql.org/docs/current/errcodes-appendix.html


(def ^:const -serialization-conflict- 40001)
(defn ex-conflict? "Returns true if `e` is an exception representing a serialization conflict."
  [e]
  (let [^SQLException e (or (clojure.stacktrace/root-cause e) e)]
    (and (instance? SQLException e)
         ;; https://www.postgresql.org/docs/current/transaction-iso.html
         (or (= -serialization-conflict- (.getErrorCode e))
             (= "40001" (.getSQLState e))))))



(def ^:const -no-transaction- 25000)
(defn ex-no-transaction? "Returns true if `e` is an exception representing missing transaction or invalid transaction state."
  [e]
  (let [^SQLException e (or (clojure.stacktrace/root-cause e) e)]
    (and (instance? SQLException e)
         (or (= -no-transaction- (.getErrorCode e))
             (= "25000" (.getSQLState e))))))



(def ^:const -input-ch-closed- 8000)
(defn ex-input-ch-closed? "Returns true if `input-ch` was closed."
  [e]
  (let [^SQLException e (or (clojure.stacktrace/root-cause e) e)]
    (and (instance? SQLException e)
         (= -input-ch-closed- (.getErrorCode e)))))



(defmacro io! "Pretty much the same as clojure.core/io!, but stricter and for our transactions here. The general idea is to put this in low-level code / components that does I/O – which will then assert that any higher level (or calling) code isn't trying to do I/O *during* a tx. I.e. all code that does I/O must be wrapped in AFTER-COMMIT or AFTER-ROLLBACK."
  [& body]
  (let [message (when (string? (first body)) (first body))
        body (if message (next body) body)]
    `(if (bound? #'*db*)
       (throw (new IllegalStateException ~(or message "I/O in transaction")))
       (do
         (when-not *in-post-tx-handling?*
           ;; FIXME: It might make sense to add a flag to this macro which will enable one to do I/O outside of tx context completely.
           (log/warn "I/O not executing in dynamic context of AFTER-COMMIT or AFTER-ROLLBACK. This might be a mistake."))
         ~@body))))



(defn run-post-commit "Runs `f` (fn [] ..) after a tx commit. NOTE: Runs outside of db and tx context."
  [f]
  (assert (bound? #'*run-post-commit*)
          "AFTER-COMMIT (or RUN-POST-COMMIT) must be used context of a transaction.")
  (case *in-post-tx-handling?*
    false (swap! *run-post-commit* conj! f)
    :commit (f)
    :rollback (throw (ex-info "AFTER-COMMIT nested inside AFTER-ROLLBACK isn't supported." {:ex-id ::tx-nesting}))))



(defmacro after-commit "Macro wrapping RUN-POST-COMMIT."
  [& body] `(run-post-commit (fn [] ~@body)))



(defn run-post-rollback "Runs `f` (fn [] ..) after a tx rollback. NOTE: Runs outside of db and tx context."
  [f]
  (assert (bound? #'*run-post-rollback*)
          "AFTER-ROLLBACK (or RUN-POST-ROLLBACK) must be used in context of a transaction.")
  (case *in-post-tx-handling?*
    false (swap! *run-post-rollback* conj! f)
    :rollback (f)
    :commit (throw (ex-info "AFTER-ROLLBACK nested inside AFTER-COMMIT isn't supported." {:ex-id ::tx-nesting}))))



(defmacro after-rollback "Macro wrapping RUN-POST-ROLLBACK."
  [& body] `(run-post-rollback (fn [] ~@body)))



(defn rollback []
  (throw (ex-info "ROLLBACK was called; user-initiated rollback." {::db-result :tx-rollback})))



(defn- rollback-notification [cause]
  (throw (ex-info "DB TX was rolled back." {::db-result :tx-rollback} cause)))



;; TODO: Add the loop that'll auto-retry (at least a few times) when we have an EX-CONFLICT? situation. If we've decided that something seems very stuck, we could bail and log the failure (fatal). Need some protocol or set of `tx-opts` for this stuff.
(defn run-sdb [tx-opts body-fn]
  (when -checks-
    (assert (not (bound? #'*db*)))
    (assert (not (bound? #'*run-post-commit*)))
    (assert (not (bound? #'*run-post-rollback*)))
    (assert (not *in-post-tx-handling?*)))

  (binding [*run-post-commit* (atom (transient [])), *run-post-rollback* (atom (transient []))]
    (let [ret-val (promise)]

      (try
        ;; TODO: We could wrap this in a Fn and we'd then be able to nest RUN-SDB blocks correctly.
        (jdbc/with-db-connection [db-conn @-db-conn-]
          (jdbc/with-db-transaction [db-conn db-conn tx-opts]
            (binding [*db* db-conn]
              (try
                (deliver ret-val (body-fn))
                (catch Throwable e ;; TODO: Is this really needed? I bet c.j.j already does something like this.
                  (jdbc/db-set-rollback-only! db-conn)
                  (throw e))))))

        (when -checks- (assert (realized? ret-val)))
        @ret-val

        (catch Throwable e
          (cond
            (= :rollback (::db-result (ex-data e))) ;; User initiated call to ROLLBACK?
            (rollback-notification e)

            (ex-conflict? e) ;; Serialization conflict? Implies a DB rollback.
            (do
              ;; NOTE: We warn about this because this is something that should not happen very often (in a well-designed system) -- and it is something that might slow down a system significantly.
              (log/warn "Serialization conflict; this tx was rolled back.")
              (rollback-notification e))

            true
            (rollback-notification e)))

        (finally
          ;; NOTE: Not bulletproof, but if someone uses threads in a wrong way things might fail a bit earlier.
          (try
            (let [post-commit-fns (swap-vals! *run-post-commit* :err-tx-closed)
                  post-rollback-fns (swap-vals! *run-post-rollback* :err-tx-closed)
                  type (if (realized? ret-val) :commit :rollback)]
              (binding [*in-post-tx-handling?* type] ;; Used by IO!.
                (doseq [f (case type
                            :commit (persistent! (post-commit-fns 0))
                            :rollback (persistent! (post-rollback-fns 0)))]
                  (f))))
            (catch Throwable e
              (log/fatal e "Post-commit and post-rollback code should catch top-level exceptions!"))))))))



(defmacro with-sdb "Executes `body` in the dynamic context of a DB connection and transaction. Returns whatever `body` returns or throws an exception if the tx rolls back. The exception will refer to a cause and a map (available via EX-DATA): {::db-result :tx-rollback}."
  [[{:keys [tx-opts]}] & body] `(run-sdb (merge {:isolation :serializable} ~tx-opts)
                                         (fn [] ~@body)))



(defn describe-table "Prints out information about a DB table or its columns."
  [table-name]
  (with (fn []
          (-> (jdbc/query *db* ["SELECT table_schema, column_name, data_type, column_default, is_nullable FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = ?;" table-name])
              (lrn/print-table-str)
              (lrn/dbg-println)))
    (if (bound? #'*db*)
      (it)
      (with-sdb [] (it)))))









;; NOTE: Unfinished async stuff below here; ignore.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; NOTE/TODO: I've dropped the async stuff for now; I'm running out of time and need to make this simpler.
#_(defn start "Starts a new DB tx which will be used to execute any Fns (fn [db-conn] ..) sent to `input-ch`.

  `db-conn`: Something (e.g. a Delay) we can deref to get a db-spec.
  `input-ch`: This is passed Fns – as described above. If this is passed :tx-rollback or :tx-commit, the tx will do as such and end. `input-ch` will be closed once the tx ends. If `input-ch` is closed early (i.e. it returns NIL here) the tx will roll back.
  `tx-type`: :read-only | :read-write  (TODO: add :commute here?)
  `tx-timeout`: The timeout for an already established and running tx. Will reset on new activity.
  `tx-result` (optional): If passed a promise, this will be sent :tx-commit, :tx-rollback, :tx-conflict (implies :tx-rollback), :tx-timeout (implies :tx-rollback) or an exception (implies :tx-rollback) on tx end.

  Returns (blocking during init): tx-id, a string representing the tx – or throws an exception."
    ([db-conn tx-type tx-timeout input-ch]
     (start db-conn tx-type tx-timeout input-ch (promise)))

    ([db-conn tx-type tx-timeout input-ch tx-result]
     (let [start-ret-val (promise), tx-id (str (uuid/v4))
           log-m {:tx-type tx-type, :tx-id tx-id, :tx-result tx-result}]
       (async/thread
         (binding [*run-post-commit* (atom (transient [])), *run-post-rollback* (atom (transient []))]
           (let [run-proms (volatile! [])] ;; TODO: !!! THIS WAS THE LAST THING I WAS WORKING ON HERE !!! I.e. I need to deliver any pending run-proms when we have an exception or rollback etc..
             (try
               (jdbc/with-db-connection [db-conn db-conn]
                 (jdbc/with-db-transaction
                   [db-conn db-conn {:read-only? (case tx-type :read-only true, :read-write false)
                                     :isolation (case tx-type :read-only :read-committed, :read-write :serializable)}]
                   (binding [*db* db-conn]
                     (try
                       (swap! -tx-id->input-ch- assoc tx-id input-ch)
                       (when -checks- (assert (not (realized? start-ret-val))))
                       (deliver start-ret-val tx-id)
                       (when -debug- (log/info "New tx started" log-m))
                       (loop []
                         (let [timeout-ch (async/timeout tx-timeout)]
                           (async/alt!!
                             input-ch ([[run-f run-prom]] ;; See the RUN Fn internals to understand what's being sent here.
                                       (swap! run-proms conj run-prom)
                                       (cond
                                         (keyword? run-f) ;; A tx-command?
                                         (case run-f
                                           ;; We can't deliver to `tx-result` early here because tx might still end up conflicting.
                                           :tx-commit (future (deliver run-prom [:tx-result @tx-result]))
                                           :tx-rollback (do (jdbc/db-set-rollback-only! db-conn)
                                                            (deliver run-prom [:tx-result :tx-rollback])
                                                            (deliver tx-result :tx-rollback)))

                                         (nil? run-f) ;; `input-ch` closed?
                                         (do (jdbc/db-set-rollback-only! db-conn)
                                             (deliver run-prom [:exception (ex-info "`input-ch` was closed." log-m
                                                                                    (SQLException. "" "08000" -input-ch-closed-))])
                                             (deliver tx-result :tx-rollback))

                                         true ;; Normal processing; execute `run-f` then recur.
                                         (do (try (deliver run-prom [:normal (run-f db-conn)])
                                                  (catch Throwable e ;; For transport of `run-prom` to the CATCH block outside.
                                                    (dbgc :db.clj e)
                                                    (throw (ex-info "During `run-f`." {:run-prom run-prom} e))))
                                             (recur))))

                             timeout-ch (do
                                          (when -debug- (log/info "Tx timing out; rolling back and closing." log-m))
                                          (jdbc/db-set-rollback-only! db-conn)
                                          (deliver tx-result :tx-timeout)))))))))

               (deliver tx-result :tx-commit) ;; NOOP if :tx-rollback etc. was already delivered.

               (catch Throwable e
                 (if (ex-conflict? e)
                   (do
                     (when -debug- (log/info "Serialization conflict; this (our) tx was rolled back." log-m))
                     (deliver tx-result :tx-conflict)
                     (when-let [run-prom (:run-prom (ex-data e))]
                       (dbg-println "delivering to run-prom:" run-prom)
                       (deliver run-prom [:exception (or (ex-cause e) e)])))
                   (do
                     #_(log/warn e "[START, top-level]" log-m) ;; FIXME: WARN or FATAL?
                     (deliver tx-result (or (ex-cause e) e))
                     (when-let [run-prom (:run-prom (ex-data e))]
                       (deliver run-prom [:exception (or (ex-cause e) e)])))))

               (finally
                 (swap! -tx-id->input-ch- dissoc tx-id)
                 (async/close! input-ch)
                 (if (realized? tx-result)
                   (try
                     (deliver start-ret-val @tx-result) ;; NOOP if already delivered (above).
                     ;; NOTE: Not bulletproof, but if someone uses threads in a wrong way things might fail a bit earlier.
                     (let [post-commit-fns (swap-vals! *run-post-commit* :err-tx-closed)
                           post-rollback-fns (swap-vals! *run-post-rollback* :err-tx-closed)]
                       (binding [*in-post-tx-handling?* true] ;; Used by IO!.
                         (doseq [f (case @tx-result
                                     :tx-commit (persistent! (post-commit-fns 0))
                                     (persistent! (post-rollback-fns 0)))]
                           (f))))
                     (catch Throwable e
                       (log/fatal e "Post-commit and post-rollback code should catch top-level exceptions!" log-m)))
                   (do ;; This shouldn't happen, but just in case.
                     (log/fatal "`tx-result` should have been realized at this point." log-m)
                     (deliver tx-result :tx-rollback)))))))) ;; Assume it rolled back so any futures above don't end up stuck.

       (let [ret-val (deref start-ret-val 15000 (TimeoutException. "Timeout while waiting for initial DB connection or TX."))]
         (if (instance? Throwable ret-val)
           (do
             (async/close! input-ch)
             (throw ret-val))
           ret-val)))))



;; NOTE/TODO: I've dropped the async stuff for now; I'm running out of time and need to make this simpler.
;; FIXME/TODO: Consider always just returning a promise?
#_(defn run "Executes `f` (fn [db-conn] ..) in context of a DB tx specified by `tx-id`. `f` can also be passed :tx-commit or :tx-rollback.
  `sync?` (optional): If false (default) RUN will not block.
  `timeout` (optional; default is 15000): Max queue + run-time in ms for processing of `f` when `sync?` is true.
  Returns: If `sync?` is true this will return the return value of `f`. Any uncaught exception in `f` will be re-thrown in context of the call to RUN.
           If `sync?` is false (the default) this will return a promise. The promise will yield one of these variants:
             - [:normal, <return value of `f`>]
             - [:tx-result, :tx-rollback | :tx-commit | | :tx-conflict | :tx-timeout]
             - [:exception, <exception>]
  Throws: ExceptionInfo possibly wrapping a cause (e.g. SQLException) – or TimeoutException when `sync?` is true."
    ([^String tx-id f]
     (run tx-id false f))

    ([^String tx-id sync? f]
     (run tx-id sync? 15000 f))

    ([^String tx-id sync? timeout f]
     (assert (not (bound? #'*db*)) "Can't nest RUN inside RUN; at least not for now.")
     (if-let [input-ch (get @-tx-id->input-ch- tx-id)]
       (let [run-prom (promise)]
         (if sync?
           (if (async/>!! input-ch [f run-prom]) ;; The sink or "other end" of `input-ch` are the internals in START.
             (let [[res-type res-val] (deref run-prom timeout
                                             [:exception (TimeoutException. "Timeout while waiting for `run-prom`.")])]
               (dbg-println 'res-type '=> res-type '| 'res-val '=> res-val)
               (case res-type
                 (:normal :tx-result) res-val
                 :exception (throw res-val)))
             (throw (ex-info "`input-ch` was closed; tx session timeout?"
                             {:tx-id tx-id} (SQLException. "" "08000" -input-ch-closed-))))
           (do ;; ASYNC mode.
             (async/put! input-ch [f run-prom]
                         #(when-not %
                            (let [e (ex-info "`input-ch` was closed; tx session timeout?"
                                             {:tx-id tx-id} (SQLException. "" "08000" -input-ch-closed-))]
                              (deliver run-prom [:exception e]))))

             run-prom)))
       (throw (ex-info "`tx-id` not found; tx session timeout?" {:tx-id tx-id}
                       (SQLException. "" "25000"))))))



#_(defn commit "If `sync?` is false (default) this will return a promise; see the RUN Fn for info about this – or use the `tx-result` promise given to START or the AFTER-COMMIT macro (or RUN-POST-COMMIT Fn)."
    ([^String tx-id] (commit tx-id false))
    ([^String tx-id sync?] (run tx-id sync? :tx-commit)))



#_(defn rollback "If `sync?` is false (default) this will return a promise; see the RUN Fn for info about this – or use the `tx-result` promise given to START or the AFTER-ROLLBACK macro (or RUN-POST-ROLLBACK Fn)."
    ([^String tx-id] (rollback tx-id false))
    ([^String tx-id sync?] (run tx-id sync? :tx-rollback)))



;; NOTE/TODO: I've dropped the async stuff for now; I need to make this simpler.
#_(defn run-sync "Same as RUN, but with `sync?` defaulting to true."
    [^String tx-id f] (run tx-id true f))


;; NOTE/TODO: I've dropped the async stuff for now; I need to make this simpler.
#_(defmacro with-db "Macro wrapping RUN."
    [[db tx-id] & body] `(run ~tx-id (fn [~db] ~@body)))


;; NOTE/TODO: I've dropped the async stuff for now; I need to make this simpler.
#_(defmacro with-sdb "Macro wrapping RUN-SYNC."
    [[db tx-id] & body] `(run-sync ~tx-id (fn [~db] ~@body)))



;; NOTE/TODO: I've dropped the async stuff for now; I'm running out of time and need to make this simpler.
#_(defmacro with-db-tx "Macro wrapping START (blocking) --> RUN (blocking) with `body` --> COMMIT (blocking) or ROLLBACK (non-blocking).
  Returns: the result of `body` when COMMIT was successful – or throws an exception (after calling ROLLBACK) otherwise."
    [[db-sym tx-id-sym tx-id] & body]
    `(let [~tx-id-sym ~tx-id]
       (try
         (let [ret-val# (with-sdb [~db-sym ~tx-id-sym]
                          ~@body)
               commit-res# (commit ~tx-id-sym true)]
           (when (not= :tx-commit commit-res#)
             (throw (ex-info "Transaction was not committed" {:commit-res commit-res#}
                             (SQLException. "" "25000"))))
           ret-val#)
         (catch Throwable e#
           (try (rollback ~tx-id-sym false)
                ;; NOTE: Should be OK; if we can't rollback at this point we've probably already rolled back.
                (catch Throwable inner#))
           (throw e#)))))


#_(with-db-tx [db txid (start @-db-conn- :read-only 5000 (async/chan))]
    (println txid)
    (println (jdbc/query db "SELECT version();"))
    (println (jdbc/query *db* "SELECT version();"))
    42)



#_(let [input-ch (async/chan)
        txid (start @-db-conn- :read-only 5000 input-ch)]
    (run-sync txid (fn [db] (println (jdbc/query db "SELECT version();")))))
