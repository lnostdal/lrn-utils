(in-ns 'lrn-utils.core)

;; Debug output with blocking buffer to make sure you don't blow up Emacs by mistake
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce -dbg-ch- (async/chan 10))


(async/go-loop []
  (try
    (when-let [[elt bnds] (async/<! -dbg-ch-)]
      (locking -dbg-ch-
        (with-bindings bnds
          (condp instance? elt
            String (println (if (> 100000 (.length ^String elt))
                              (subs elt 0 (min (.length ^String elt) 100000))
                              elt))
            Throwable (io.aviso.exception/write-exception elt)
            (pprint (gist elt)))
          (flush))
        (Thread/sleep 25))) ;; TODO: FLUSH will block and work as a sort of rate limiter anyway, no?
    (catch Throwable e
      (println "[lrn-utils.core/-dbg-ch-]:" e)
      (Thread/sleep 1000)))
  (recur))


(defn dbg-println [& xs]
  (async/>!! -dbg-ch- [(apply print-str xs) (get-thread-bindings)])
  nil)

(defn dbg-pprint
  ([] ;; Transducer variant.
   (fn [xf]
     (fn ([] (xf))
       ([result] (xf result))
       ([result obj]
        (dbg-pprint obj)
        (xf result obj)))))

  ([o] ;; Normal variant.
   (async/>!! -dbg-ch- [(pprint-str (gist o)) (get-thread-bindings)])))


(defmacro dbg "Quick inline debugging where other stuff will or might provide context."
  [x] `(let [res# ~x]
         (dbg-println (str (pprint-str '~x) " => " (pprint-str res#)))
         res#))


(defmacro dbgg "Quick inline debugging where other stuff will or might provide context."
  [x] `(let [res# ~x]
         (dbg-println (str (pprint-str '~x) " => " (pprint-str (gist res#))))
         res#))


(defmacro dbgf "Quick inline debugging with context from `ctx` and meta-environment."
  [ctx x]
  (let [m (meta &form)]
    `(let [res# ~x]
       (dbg-println (str "#DBGF " ~ctx " (" (last (str/split ~*file* #"/")) ":" ~(:line m) ":" ~(:column m) ") ===>" \newline
                         (pprint-str '~x) " => " (pprint-str (gist res#)) \newline))
       res#)))


(defmacro dbg-time "Quick inline debugging with evaluation time (similar to clojure.core/time) included."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         diff# (/ (double (- (. System (nanoTime)) start#))
                  1000000.0)]
     (dbg-println (str (pprint-str '~expr) " ==[" diff# "ms]==> "
                       (pprint-str (gist ret#))))
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
