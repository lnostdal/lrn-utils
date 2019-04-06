(in-ns 'lrn-utils.core)

;; Debug output with blocking buffer to make sure you don't blow up Emacs by mistake
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn print-or-pprint-str
  [x] (if (string? x) x (pprint-str x)))


(defonce -dbg-max-string-length- 100000)
(defonce -dbg-ch-
  (with1 (async/chan 50)
    (async/go-loop []
      (try
        (when-let [[elt bnds] (async/<! it)]
          (with-bindings bnds
            (condp instance? elt
              String (println (if (> ^long -dbg-max-string-length- (.length ^String elt))
                                (subs elt 0 (min (.length ^String elt) ^long -dbg-max-string-length-))
                                elt))
              Throwable (io.aviso.exception/write-exception elt)
              (print-or-pprint-str (gist elt)))
            (flush))
          (Thread/sleep 25)) ;; TODO:
        (catch Throwable e
          (println "[lrn-utils.core/-dbg-ch-]:" e)
          (Thread/sleep 1000)))
      (recur))))


(defn to-dbg-ch [x] (async/>!! -dbg-ch- [x (get-thread-bindings)]))


(defn dbg-println "Similar to PRINTLN, but runs each elt in `xs` through a call to GIST – and returns the last value in `xs`."
  [& xs]
  (to-dbg-ch (transduce (comp (map gist)
                              (map print-or-pprint-str)
                              (interpose \space))
                        rfs/str ;; StringBuilder > new String
                        "" xs))
  (last xs))


(defn dbg-pprint
  ([] ;; Transducer variant.
   (fn [xf]
     (fn ([] (xf))
       ([result] (xf result))
       ([result obj]
        (dbg-pprint obj)
        (xf result obj)))))

  ([o] ;; Normal variant.
   (to-dbg-ch o)
   o))


(defmacro dbg "Quick inline debugging where other stuff will or might provide context."
  [x]
  `(let [res# ~x]
     (to-dbg-ch (str (pprint-str '~x) " => " (print-or-pprint-str (gist res#))))
     res#))


(defmacro dbgc "Quick inline debugging with brief context denoted by `ctx`."
  [ctx x]
  `(let [res# ~x]
     (to-dbg-ch (str (pprint-str ~ctx) " | " (pprint-str '~x) " => " (print-or-pprint-str (gist res#))))
     res#))


(defmacro dbgf "Quick inline debugging with context from `ctx` and meta-environment."
  [ctx x]
  (let [m (meta &form)]
    `(let [res# ~x]
       (to-dbg-ch (str "#DBGF " ~ctx " (" (last (str/split ~*file* #"/")) ":" ~(:line m) ":" ~(:column m) ") ===>" \newline
                       (pprint-str '~x) " => " (print-or-pprint-str (gist res#)) \newline))
       res#)))


(defmacro dbg-time "Quick inline debugging with evaluation time (similar to clojure.core/time) included."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         diff# (/ (double (- (. System (nanoTime)) start#))
                  1000000.0)]
     (to-dbg-ch (str (pprint-str '~expr) " ==[" diff# "ms]==> "
                     (print-or-pprint-str (gist ret#))))
     ret#))



;; Store actual objects in a "debug cache"
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: It'd be cool if this used maximumWeight and https://github.com/clojure-goes-fast/clj-memory-meter instead of simply maximumSize.
(defonce ^Cache -debug-cache- (-> (CacheBuilder/newBuilder)
                                  (.maximumSize 1000) ;; TODO: Magic value!
                                  (.build)))

(defn dbg-clear []
  (.clear (.asMap -debug-cache-)))

(defn dbg-put [id item]
  (locking -debug-cache-
    (if-let [^EvictingQueue existing-items (.getIfPresent -debug-cache- id)]
      (.add existing-items [(jtime/instant) item])
      (.put -debug-cache- id (with1 (EvictingQueue/create 10) (.add it [(jtime/instant) item])))))) ;; TODO: 10 is magic value here!

(defn dbg-get
  ;; NOTE: Even though REVERSE is eager and realizes the entire coll in question, it should be OK since EvictingQueue limits this.
  ([] (reduce-kv #(assoc %1 %2 (reverse %3)) (sorted-map) (.asMap -debug-cache-)))

  ([id] (reverse (.getIfPresent -debug-cache- id))))
