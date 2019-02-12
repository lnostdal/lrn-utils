(ns lrn-utils.core
  (:require [clojure.pprint :refer (cl-format print-table #_pprint)])
  (:require [zprint.core :refer (czprint czprint-str) :rename {czprint pprint, czprint-str pprint-str}])
  (:require [clojure.string :as str])
  (:require [clojure.core.async :as async])
  (:require [java-time :as jtime])
  (:require io.aviso.exception)

  (:import (com.google.common.collect EvictingQueue)
           (com.google.common.cache Cache CacheBuilder CacheLoader)))


;; TODO: Remove this when I pass bindings during debugging again. I.e. _external_ context should define these things..
(zprint.core/set-options! {:width 130, :max-length 50, :max-depth 8}) ;; 270 is full length of Emacs window. 130 is 2-column length.



(defn atype ;; From: https://gist.github.com/Chouser/a571770f06ef2a9c5334/
  "Return a string representing the type of an array with `dims` dimentions and an element of type `klass`. For primitives, use a `klass` like Integer/TYPE. Useful for type hints of the form: ^#=(atype String) my-str-array"
  ([klass] (atype klass 1))
  ([klass dims]
   (.getName (class
              (apply make-array
                     (if (symbol? klass) (eval klass) klass)
                     (repeat dims 0))))))



(defmacro do1 "As PROG1 from Common Lisp."
  [x & body]
  `(let [x# ~x]
     ~@body
     x#))

(defmacro do2 "As PROG2 from Common Lisp."
  [x y & body]
  `(let [y# ~y]
     ~@body
     y#))


(defmacro with [form & body]
  `(let [~'it ~form]
     ~@body))

(defmacro with1 "As DO1, but anaphoric."
  [form & body]
  `(with ~form
     ~@body
     ~'it))



(defmacro in "Is `x` equal (=) to any of the elements in `args`?
  E.g. ```(in :filled :new :filled) => true``` or ```(in :cancelled :new :filled) => false```"
  [x & args]
  (let [it (gensym)]
    `(let [~it ~x]
       (or ~@(map (fn [y] `(= ~it ~y))
                  args)))))



(defn print-table-str ^String [& xs]
  (with-out-str (apply print-table xs)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti extract-gist "Extracts the \"most interesting\" stuff from `o` -- e.g. suitable for passing to some pretty printer."
  (fn [o & _] (class o)))

(defmethod extract-gist :default [o]
  o)

(defmethod extract-gist java.lang.Class [^java.lang.Class c]
  (symbol (second (str/split (.toString c) #" "))))

(defmethod extract-gist java.lang.String [^java.lang.String o]
  (subs o 0 (min (.length o) 1000))) ;; TODO: Magic nr.. This should probably be pulled from a dynamic var.

(defmethod extract-gist java.lang.Throwable [^java.lang.Throwable o]
  (with-out-str (io.aviso.exception/write-exception o)))

(defn gist "Returns the 'gist' of some object. Usually a shorter or more informative (for humans) version of the object. Note that text is not necessarily returned here."
  [o] (clojure.walk/prewalk extract-gist o))



;; Debug output with blocking buffer to make sure you don't blow up Emacs by mistake
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce -dbg-ch- (async/chan 10))


(let [repl-out (get (get-thread-bindings) #'*out*)] ;; TODO: Good idea? I could pass *out* from DBG-PRINTLN etc..
  (async/go-loop []
    (try
      (when-let [elt (async/<! -dbg-ch-)]
        (locking -dbg-ch-
          (binding [*out* repl-out]
            (condp instance? elt
              String (println (subs elt 0 (min (.length ^String elt) 100000))) ;; TODO: Magic nr..
              Throwable (io.aviso.exception/write-exception elt)
              (pprint (gist elt)))
            (flush))
          (Thread/sleep 25))) ;; TODO: FLUSH will block and work as a sort of rate limiter anyway, no?
      (catch Throwable e
        (println "[lrn-utils.core/-dbg-ch-]:" e)
        (Thread/sleep 1000)))
    (recur)))


(defn dbg-println [& xs]
  (async/>!! -dbg-ch- (apply print-str xs))
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
   (async/>!! -dbg-ch- (pprint-str (gist o)))))


(defmacro dbg "Quick inline debugging where other stuff will or might provide context."
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



(defn do1-sleep "Inline sleep often used in context of -> macro or similar process. Returns `obj`."
  ([millis]
   (fn [xf] ;; Transducer variant.
     (fn
       ([] (xf))
       ([result] (xf result))
       ([result obj]
        (Thread/sleep millis)
        (xf result obj)))))

  ([obj millis] ;; Normal variant.
   (do1 obj
     (Thread/sleep millis))))


(defn do2-sleep "Inline sleep often used in context of ->> macro or similar process. Returns `obj`."
  [millis obj]
  (do1 obj
    (Thread/sleep millis)))



(defn uuid "Returns a universally unique ID."
  ^String [] (.toString (java.util.UUID/randomUUID)))



(let [id (atom Long/MIN_VALUE)]
  (defn uid "Returns a per-session unique and thread-safe ID."
    ^long [] (swap! id unchecked-inc)))



(defn url-encode-component ^String [^String s]
  (.replace (java.net.URLEncoder/encode s "UTF-8")
            "+"
            "%20"))


(defn url-decode-component ^String [^String s]
  (java.net.URLDecoder/decode s "UTF-8"))



(defn mime-encode-rfc-2047 ^String [^String s]
  (str "=?UTF-8?Q?"
       (-> (url-encode-component s)
           (str/replace "%20" "_")
           (str/replace "%" "="))
       "?="))



(defn sha ^String [^String input-str]
  (let [md (java.security.MessageDigest/getInstance "SHA-512")]
    (. md update (.getBytes input-str))
    (let [digest (.digest md)]
      ;; TODO: Not the best or fastest way to do this. x)
      (reduce str (map #(format "%02x" (bit-and ^byte % 0xff)) digest)))))



(defn async-consume-buffer
  "Consume as many values from `ch` as possible without blocking. Once `ch` blocks (i.e. its buffer is empty), the values are returned as a vector.
  `first-request-blocking?`: If true (default when not supplied is false), the first request (only) from `ch` is permitted to block."
  ([ch] (async-consume-buffer ch false))

  ([ch first-request-blocking?]
   (loop [e (if first-request-blocking?
              (async/<!! ch)
              (async/poll! ch))
          v (transient [])]
     (if e
       (recur (async/poll! ch) (conj! v e))
       (persistent! v)))))



;; Try to make java.time.* and the clojure.java-time library less annoying to use
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   * (clj-time.core/now) is now (java-time/instant)
;;   * (time.coerce/to-long (time/now)) => (ts-to-long (jtime/local-date-time))
;;   * (time/seconds 30) => (jtime/duration 30 :seconds)
;;   * (time/in-seconds (time/interval (time/now) (time/seconds 5))) =>
;;       (jtime/as (jtime/duration (jtime/local-date-time)
;;                                 (jtime/plus (jtime/local-date-time) (jtime/duration 5 :seconds)))
;;                 :seconds)
;;  * (time/floor .. time/day) => (jtime/truncate-to .. :days)

(defn to-ts "Best effort attempt at converting what's given for `i` into a java.time.* type -- usually java.time.Instant and always UTC."
  ;; TODO/NOTE: Tries to use java.time.* classes and methods directly, because whatever clojure.java-time is doing(???) seems slow. I might change some of this around later...
  [i]
  (condp instance? i
    java.time.Instant i
    java.time.Duration i
    java.time.ZonedDateTime (.toInstant ^java.time.ZonedDateTime i)
    java.time.LocalDateTime (.toInstant ^java.time.LocalDateTime i java.time.ZoneOffset/UTC)
    java.time.LocalDate (-> (.atStartOfDay ^java.time.LocalDate i) ;; => LocalDateTime
                            (.toInstant java.time.ZoneOffset/UTC))
    java.time.LocalTime (-> (.atDate ^java.time.LocalTime i (java.time.LocalDate/now)) ;; => LocalDateTime
                            (.toInstant java.time.ZoneOffset/UTC))
    java.lang.Long (java.time.Instant/ofEpochMilli i)
    String (try ;; NOTE: This is crazy slow; use explicit format instead!
             (to-ts (jtime/local-date-time i)) ;; E.g. "2019-02-11T18:47:58"
             (catch clojure.lang.ExceptionInfo e ;; ..why would anyone try to "hide" such an important exception???
               (if (instance? java.time.DateTimeException (ex-cause e))
                 ;; E.g. "2018-01-01". NOTE: "2018-1-1" won't work!
                 (to-ts (jtime/truncate-to (jtime/local-date-time (jtime/local-date i)) :days))
                 (throw e))))))


(defn to-time [i] (to-ts i)) ;; TODO. Remove later.

(defn ts-to-long " * java.time.Instant => millis sine epoch.
  * java.time.Duration => millis"
  ^long [i]
  (condp instance? i
    java.lang.Long i
    java.time.Instant (.toEpochMilli ^java.time.Instant i)
    java.time.Duration (.toMillis ^java.time.Duration i)
    (ts-to-long (to-ts i))))


(defn ts-now-long ^long []
  (System/currentTimeMillis))

(defn ts-to-str
  (^String [i] (ts-to-str i (-> (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss.SSS")
                                (.withZone java.time.ZoneOffset/UTC))))
  (^String [i ^java.time.format.DateTimeFormatter fmt]
   (.format fmt (to-ts i))))


(defmethod extract-gist java.time.LocalDateTime [^java.time.LocalDateTime o]
  (ts-to-str o))




(defn upmap "Same as MAP, but eager and with threads and unordered execution (unlike PMAP). The return values are ordered however. E.g.:
  (upmap (fn [f] (f))
       (map (fn [x] (fn [] (Thread/sleep x) (dbg x)))
            [3000 2000 1000]))
  x => 1000
  x => 2000
  x => 3000
  (3000 2000 1000)"
  [f coll]
  (map deref (mapv #(future (f %))
                   coll)))



(defn lazy-mapcat
  "https://stackoverflow.com/questions/21943577/mapcat-breaking-the-lazyness"
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (concat (f (first s)) (lazy-mapcat f (rest s))))))



(let [decfmt (java.text.DecimalFormat. "#.########")]
  (defn double-hstr "Converts `x` to a \"nice looking\" floating point number (string) suitable for human readers."
    ^String [^double x] (.format decfmt x)))



(defn double-finite-or-false "Returns `n` if it is a double finite number (Double/isFinite) or FALSE otherwise."
  ^double [^double n]
  (if (Double/isFinite n)
    n
    false))



(defn double-finite-or-zero ^double [^double n]
  (if (Double/isFinite n)
    n
    0.0))



(defn double-zero-to-nan ^double [^double n]
  (if (zero? n)
    ##NaN
    n))



(defn queue "Persistent immutable queue."
  (^clojure.lang.PersistentQueue []
   (queue nil))

  (^clojure.lang.PersistentQueue [coll]
   (loop [coll coll, q (clojure.lang.PersistentQueue/EMPTY)]
     (if (empty? coll)
       q
       (recur (next coll) (conj q (first coll)))))))



(deftype GCedResource ;; Clojure WITH-OPEN macro (sort of?) on drugs. Bonus: It will work in context of laziness.
    [resource]

  Object
  (finalize [o]
    #_(info "[GCedResource, finalize]:" resource)
    (.close resource)))



(defn filter-last "Fast variant of doing (LAST (FILTER ..)) via RSEQ.
  `v`: A vector or something RSEQ can handle efficiently."
  [pred v] (first (filter pred (rseq v))))



(defmacro afirst [arr]
  `(aget ~arr 0))


(defmacro alast [arr]
  `(let [arr# ~arr]
     (aget arr# (- (alength arr#) 1))))


(defmacro aiter "Iterate over each element in array `arr` and execute `body` with element bound to `var`."
  [[var arr] & body]
  `(let [len# (alength ~arr)]
     (loop [idx# 0]
       (if (= idx# len#)
         nil
         (do
           (let [~var (aget ~arr idx#)]
             ~@body)
           (recur (+ idx# 1)))))))



;; These are significantly (sometimes) faster than e.g. ATOM or VOLATILE!.

(definterface ILong
  (^long addVal [^long v])
  (^long subVal [^long v])
  (^long setVal [^long v])
  (^long getVal []))

(deftype OLong
    [^:unsynchronized-mutable ^long val]

  ILong
  (addVal [o v] (set! val (+ val v)))
  (subVal [o v] (set! val (- val v)))
  (setVal [o v] (set! val v))
  (getVal [o] val))



(definterface IDouble
  (^double addVal [^double v])
  (^double subVal [^double v])
  (^double setVal [^double v])
  (^double getVal []))

(deftype ODouble
    [^:unsynchronized-mutable ^double val]

  IDouble
  (addVal [o v] (set! val (+ val v)))
  (subVal [o v] (set! val (- val v)))
  (setVal [o v] (set! val v))
  (getVal [o] val))



(definterface IBoolean
  (^boolean setVal [^boolean v])
  (^boolean getVal []))

(deftype OBoolean
    [^:unsynchronized-mutable ^boolean val]

  IBoolean
  (setVal [o v] (set! val v))
  (getVal [o] val))



(definterface IOObject
  (setVal [new-val])
  (getVal [])
  (oswap [f])
  (oswap [f x])
  (oswap [f x y])
  (oswap [f x y z]))

(deftype OObject
    [^:unsynchronized-mutable val]

  IOObject
  (setVal [o new-val] (set! val new-val))
  (getVal [o] val)
  (oswap [o f] (set! val (f val)))
  (oswap [o f x] (set! val (f val x)))
  (oswap [o f x y] (set! val (f val x y)))
  (oswap [o f x y z] (set! val (f val x y z)))

  clojure.lang.IDeref
  (deref [o] val))



(defmacro jiter "Use a java.util.Iterator to iterate over `coll`. `jiterator` will be visible in body; you can call e.g. .remove on this. Returns NIL."
  [[var coll] & body]
  `(let [^java.util.Iterator iter# (.iterator ~coll)
         ~'jiterator iter#]
     (while (.hasNext iter#)
       (let [~var (.next iter#)]
         ~@body))))

(defmacro vec-iter "Simple iteration over a Vector or perhaps anything COUNTED? with lookup by index via IFn. Returns NIL."
  [[var coll] & body]
  `(let [coll# ~coll
         length# (count coll#)]
     (dotimes [idx# length#]
       (let [~var (coll# idx#)]
         ~@body))))

(defmacro doiter "Pretty much the same as RUN!, but with the parameter list reordered. Does not hold on to the head of a lazy sequence. Returns NIL."
  [[var coll] & body]
  `(run! (fn [~var] ~@body)
         ~coll))



(defn double-mantissa "NOTE: Not accurate."
  ^double [^double x]
  (if (pos? x)
    (- x (Math/floor x))
    (- x (Math/ceil x))))



;; TODO: Better name? Make hour a param?
(defn ts-floor "Align `ts` on the hourly split into `interval` segments.
  `ts`: Millis.
  `interval`: Millis.

  Returns: timestamp in millis."
  ^long [^long ts ^long interval]
  ;; Old slower (I think!) variant using clj-time.
  #_(let [ts (time.coerce/from-long ts) ;; TODO: Calculate this stuff using ^long as is!
          ts-floor (time/floor ts time/hour)
          ts-dlt (long (time/in-millis (time/interval ts-floor ts)))
          ts-mlt (long (Math/floor (/ ts-dlt interval)))]
      (time.coerce/to-long (time/plus ts-floor (time/millis (* ts-mlt interval)))))
  (let [hour 3600000
        ts-floored-hour (- ts (rem ts hour))
        ts-dlt (- ts ts-floored-hour)
        ts-mlt (long (/ ts-dlt interval))]
    (+ ts-floored-hour (* ts-mlt interval))))



;; Allows you to do e.g. (reduce-kv (fn [m k v] ..) {} (java.util.HashMap.))
;; NOTE: Remove this when/if https://dev.clojure.org/jira/browse/CLJ-1762 is closed.
(extend-protocol clojure.core.protocols/IKVReduce ;; PS: Isn't this awesome? x)
  java.util.Map
  (kv-reduce
    [amap f init]
    (let [^java.util.Iterator iter (.. amap entrySet iterator)]
      (loop [ret init]
        (if (.hasNext iter)
          (let [^java.util.Map$Entry kv (.next iter)
                ret (f ret (.getKey kv) (.getValue kv))]
            (if (reduced? ret)
              @ret
              (recur ret)))
          ret)))))



(defn small-vec ;; TODO: Figure out the best way to do this later; point is I'll have a single location where I deal with this.
  "Returns a VEC aliasing a JVM array."
  (^clojure.lang.PersistentVector [a] (vec (into-array Object [a])))
  (^clojure.lang.PersistentVector [a b] (vec (into-array Object [a b])))
  (^clojure.lang.PersistentVector [a b c] (vec (into-array Object [a b c]))))



(defn file-atomic-move "Atomically move `src` file to `trg` file -- also replacing it if needed."
  [src trg]
  (let [src (java.nio.file.Paths/get (java.net.URI/create (str "file://" src)))
        trg (java.nio.file.Paths/get (java.net.URI/create (str "file://" trg)))]
    (java.nio.file.Files/move src trg (into-array java.nio.file.CopyOption
                                                  [(java.nio.file.StandardCopyOption/ATOMIC_MOVE)
                                                   (java.nio.file.StandardCopyOption/REPLACE_EXISTING)]))))
