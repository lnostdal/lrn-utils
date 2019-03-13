(in-ns 'lrn-utils.core)


(definline elt-of? "Is `e` an element of `coll`?"
  [coll e] `(not (= -1 (.indexOf ~coll ~e))))



(defn lazy-mapcat
  "https://stackoverflow.com/questions/21943577/mapcat-breaking-the-lazyness"
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (concat (f (first s)) (lazy-mapcat f (rest s))))))



(defn queue "Persistent immutable queue."
  (^clojure.lang.PersistentQueue []
   (queue nil))

  (^clojure.lang.PersistentQueue [coll]
   (loop [coll coll, q (clojure.lang.PersistentQueue/EMPTY)]
     (if (empty? coll)
       q
       (recur (next coll) (conj q (first coll)))))))


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



(defn small-vec ;; TODO: Figure out the best way to do this later; point is I'll have a single location where I deal with this.
  "Returns a VEC aliasing a JVM array."
  (^clojure.lang.PersistentVector [a] (vec (into-array Object [a])))
  (^clojure.lang.PersistentVector [a b] (vec (into-array Object [a b])))
  (^clojure.lang.PersistentVector [a b c] (vec (into-array Object [a b c]))))



(defn catvec
  ([s] (catvec [] s))

  ([init s]
   (persistent! (reduce #(reduce conj! %1 %2)
                        (transient init)
                        s))))

(definline vec-first "Returns the first element in `v`."
  [v] `(get ~v 0))
