(in-ns 'lrn-utils.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: This thing is probably very silly and dangerous; i.e. you might end up in infinite loops because it doesn't use a dynamic var to keep track of where it has already been etc.. To do this stuff proper, the dynamic var must hold a map containing places it has already been -- and when it revisits a place it must print out some symbol or marker that signals that we're dealing with circularity.

(defmulti extract-gist "Extracts the \"most interesting\" stuff from `o` -- e.g. suitable for passing to some pretty printer."
  (fn [o & _] (class o)))

(defmethod extract-gist :default [o]
  ;; Since clojure.walk/prewalk might assoc a value of different type to a field in `o` we need to dodge any type checks.
  (if (record? o)
    (extract-gist (into {} o))
    o))

(defmethod extract-gist java.lang.Class [^java.lang.Class c]
  (symbol (second (str/split (.toString c) #" "))))

(defmethod extract-gist java.lang.String [^java.lang.String o]
  (subs o 0 (min (.length o) 1000))) ;; TODO: Magic nr.. This should probably be pulled from a dynamic var.

(defmethod extract-gist clojure.lang.Ratio [^clojure.lang.Ratio o]
  (extract-gist (Double/parseDouble (double-hstr (double o)))))

(defmethod extract-gist java.lang.Double [^java.lang.Double o]
  (with (double-hstr o)
    (try
      (java.lang.Long/parseLong it)
      (catch java.lang.NumberFormatException e
        (java.lang.Double/parseDouble it)))))

(defmethod extract-gist java.lang.Throwable [^java.lang.Throwable o]
  (with-out-str (io.aviso.exception/write-exception o)))

(defn gist "Returns the 'gist' of some object. Usually a shorter or more informative (for humans) version of the object. Note that text is not necessarily returned here."
  [o] (clojure.walk/prewalk extract-gist o))


