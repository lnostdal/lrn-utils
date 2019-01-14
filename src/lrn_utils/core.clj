(ns lrn-utils.core
  (:require [puget.printer :refer (cprint) :rename {cprint pprint}])
  (:require [clojure.string :as str])
  (:require [clojure.core.async :as async])
  (:require [clj-time.core :as time])
  (:require [clj-time.coerce :as time.coerce])
  )



(defmacro do1 [x & body]
  "As PROG1 from Common Lisp."
  `(let [x# ~x]
     ~@body
     x#))



(defmacro with [form & body]
  `(let [~'it ~form]
     ~@body))



(defmacro with1 [form & body]
  `(with ~form
     ~@body
     ~'it))



(defmacro in [x & args]
  "Is `x` equal (=) to any of the elements in `args`?
E.g. ```(in :filled :new :filled) => true``` or ```(in :cancelled :new :filled) => false```"
  (let [it (gensym)]
    `(let [~it ~x]
       (or ~@(map (fn [y] `(= ~it ~y))
                  args)))))



(defonce -dbg-locker- (Object.))
(defmacro dbg [x]
  "Quick inline debugging where other stuff will or might provide context."
  `(let [res# ~x]
     (locking -dbg-locker- ;; Try to generate better output when doing threading.
       (println (str (puget.printer/cprint-str '~x) " => " (puget.printer/cprint-str res#))))
     res#))



(defmacro dbgf [ctx x]
  "Quick inline debugging where other stuff with context from `ctx` and meta-environment."
  (let [m (meta &form)]
    `(let [res# ~x]
       (locking -dbg-locker- ;; Try to generate better output when doing threading.
         (println (str "[" ~ctx " "
                       (last (str/split ~*file* #"/"))
                       ":" ~(:line m) ":" ~(:column m) "]: "
                       (puget.printer/cprint-str '~x) " => " (puget.printer/cprint-str res#))))
       res#)))



(defn uuid "Returns a universally unique ID."
  ^String []
  (.toString (java.util.UUID/randomUUID)))



(let [id (atom Long/MIN_VALUE)]
  (defn uid "Returns a per-session unique ID."
    (^long []
     (swap! id inc))))



(defn ^String url-encode-component [^String s]
  (.replace (java.net.URLEncoder/encode s "UTF-8")
            "+"
            "%20"))


(defn ^String url-decode-component [^String s]
  (java.net.URLDecoder/decode s "UTF-8"))



(defn ^String mime-encode-rfc-2047 [^String s]
  (str "=?UTF-8?Q?"
       (-> (url-encode-component s)
           (str/replace "%20" "_")
           (str/replace "%" "="))
       "?="))



(defn sha ^String [^String input-str]
  (let [md (java.security.MessageDigest/getInstance "SHA-512")]
    (. md update (.getBytes input-str))
    (let [digest (.digest md)]
      (apply str (map #(format "%02x" (bit-and % 0xff)) digest)))))



(defn async-consume-buffer
  "Consume as many values from `ch` as possible without blocking. Once `ch` blocks (i.e. its buffer is empty), the values are returned as a vector.
  `first-request-blocking?`: If true (default when not supplied is false), the first request (only) from `ch` is permitted to block."
  ([ch]
   (async-consume-buffer ch false))

  ([ch first-request-blocking?]
   (loop [e (if first-request-blocking?
              (async/<!! ch)
              (async/poll! ch))
          v (transient [])]
     (if e
       (recur (async/poll! ch) (conj! v e))
       (persistent! v)))))



(defn to-time ^org.joda.time.DateTime [i]
  (condp instance? i
    org.joda.time.DateTime i
    java.lang.Long (time.coerce/from-long i)
    String (time.coerce/from-string i)
    org.joda.time.LocalDate i
    org.joda.time.LocalDateTime i))



(defn upmap [f coll]
  "Same as MAP, but eager and with threads and unordered execution (unlike PMAP). The return values are ordered however. E.g.:
(upmap (fn [f] (f))
       (map (fn [x] (fn [] (Thread/sleep x) (dbg x)))
            [3000 2000 1000]))
x => 1000
x => 2000
x => 3000
(3000 2000 1000)"
  (map deref (mapv #(future (f %))
                   coll)))



(defn lazy-mapcat [f coll]
  "https://stackoverflow.com/questions/21943577/mapcat-breaking-the-lazyness"
  (lazy-seq
   (when-let [s (seq coll)]
     (concat (f (first s)) (lazy-mapcat f (rest s))))))



(let [decfmt (java.text.DecimalFormat. "#.########")]
  (defn double-hstr ^String [^double x] ;; TODO: Rename to DOUBLE-HSTR.
    "Converts `x` to a \"nice looking\" floating point number (string) suitable for human readers."
    (.format decfmt x)))


(defn double-finite-or-false "Returns `n` if it is a double finite number (Double/isFinite) or FALSE otherwise."
  [^double n]
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



(defn filter-last [pred v]
  "Fast variant of doing (LAST (FILTER ..)) via RSEQ.
`v`: A vector or something RSEQ can handle efficiently."
  (first (filter pred (rseq v))))



(defmacro afirst [arr]
  `(aget ~arr 0))



(defmacro alast [arr]
  `(let [arr# ~arr]
     (aget arr# (- (alength arr#) 1))))




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
