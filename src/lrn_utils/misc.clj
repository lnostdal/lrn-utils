(in-ns 'lrn-utils.core)


(defn print-table-str ^String [& xs]
  (with-out-str (apply print-table xs)))

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



(defn keywordize ":hello_world => :hello-world or \"hello_world\" => :hello-world"
  ^clojure.lang.Keyword [x] (keyword (clojure.string/replace (name x) \_ \-)))

(defn unkeywordize ":hello-world => \"hello_world\""
  ^String [x] (clojure.string/replace (name x) \- \_))
