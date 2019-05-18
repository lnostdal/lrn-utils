(ns lrn-utils.db-queue
  "Trivial queue in PostgreSQL. For routing of messages etc. in a distributed or networked system.

  Goals / features:

    * Guaranteed delivery.
    * No double delivery if you wrap this in the tools found in the `nounce` namespace.
    * Fast enough thanks to SKIP LOCKED (9.5+). 7k messages per second is mentioned: https://tnishimura.github.io/articles/queues-in-postgresql/#performance
    * Unordered delivery.
    * Runs inside a general PostgreSQL transaction and so there's no need to coordinate two or more tx systems.
    * Bonus: no need to deal with port forwarding and related issues; everyone just talks via a single shared \"router\"; the DB.

  Based on SKIP LOCKED mentioned here: https://blog.2ndquadrant.com/what-is-select-skip-locked-for-in-postgresql-9-5/
  Cool video: https://www.youtube.com/watch?v=B81nQLg4RuU"
  (:refer-clojure :exclude [pop peek])
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.core.async :as async]
            [lrn-utils.db :as db])
  (:use lrn-utils.core))



;; NOTE: For the queue itself, READ COMMITTED is sufficient. If you use SERIALIZABLE you may face high rates of serialization failures.
;; TODO: Use some PL/pgSQL to define some nice functions in the DB itself so other users (from Python etc.) can use this trivially.
;; TODO: Adding support for `topic` would be trivial, but how useful is it? Why not just use several queues?
;; TODO: Add some "magic" (but optional!) via a c.c.async and a background worker or go block?
;; TODO: Implement LISTEN / NOTIFY and/or make a simple polling mechanism here => https://gist.github.com/mikeball/ba04dd5479f51c00205f
;; NOTE: Seems keeping old entries around will degrade performance over time -- because SKIP LOCKED implies a full scan. I.e. user needs to use a separate table for history. https://tnishimura.github.io/articles/queues-in-postgresql/#keep-the-queue-table-small



(def -col-prefix- "_emma_queue_")
(defn col-name ^String [s] (str -col-prefix- s))
(def -sys-colnames- (mapv keyword [(col-name "id")]))



#_(defn ddl-init-schema "Initialize the schema where queues will be stored."
    ([] (ddl-init-schema db/*db*))
    ([db] (jdbc/execute! db (str "CREATE SCHEMA IF NOT EXISTS " -schema- ";"))))



(defn ddl-queue "Creates a new queue."
  ([queue-id cols] (ddl-queue db/*db* queue-id cols))
  ([db queue-id cols]
   (->> (reduce conj [[(col-name "id") :bigserial "PRIMARY KEY"]
                      [(col-name "produced") :timestamp "NOT NULL DEFAULT 'now()'"]]
                cols)
        (jdbc/create-table-ddl queue-id)
        (jdbc/execute! db))))



(defn push "Pushes an `item` to `queue-id`. Returns the entry as added to the DB."
  ([queue-id item] (push db/*db* queue-id item))
  ([db queue-id item]
   (let [res (jdbc/insert! db queue-id item)]
     (apply dissoc (first res) -sys-colnames-))))



(defn pop "Pops an item from `queue-id`."
  ([queue-id] (pop db/*db* queue-id))
  ([db queue-id]
   (let [id-col (col-name "id")
         res (jdbc/query db (str "DELETE FROM " (db/identifier queue-id) " WHERE " id-col " = ("
                                 " SELECT " id-col " FROM " (db/identifier queue-id)
                                 " ORDER BY " id-col " FOR UPDATE SKIP LOCKED LIMIT 1 )"
                                 " RETURNING *;"))]
     (apply dissoc (first res) -sys-colnames-))))



(defn peek "Peeks at the next item in `queue-id`."
  ([^String queue]
   (assert false "FIXME!")))



#_(defn to-ch "Consumes from `queue-id` and sends the items to `output-ch`."
    [queue-id output-ch]
    ;; * Create an internal go-block for consumption from `queue-id` if one doesn't exist already.
    ;; * Add `output-ch` as a tap for the mult handled by the go-block.
    ;; * Keep track of each `output-ch` added -- and stop the go-block once the last `output-ch` is closed.
    )



(defn- do-test []
  (dbg-println "-------------------------------------------------------------------------------------------")
  (db/with-sdb []
    (ddl-queue :test_messages [[:message :text]]))

  (db/with-sdb []
    (mapv #(push :test_messages %1)
          (map #(hash-map :message (str "Hello World nr. " %))
               (range 5))))

  (dotimes [i 5]
    (dbg-println (db/with-sdb []
                   (pop :test_messages))))

  (db/with-sdb []
    (jdbc/execute! db/*db* (jdbc/drop-table-ddl :test_messages))))
