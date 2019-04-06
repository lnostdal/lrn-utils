(in-ns 'lrn-utils.core)

;; These are significantly (sometimes) faster than e.g. ATOM or VOLATILE!. Don't use unless you do not understand what :unsynchronized-mutable means.


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





