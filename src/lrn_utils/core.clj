(ns lrn-utils.core
  (:import (com.google.common.collect EvictingQueue)
           (com.google.common.cache Cache CacheBuilder CacheLoader))
  (:require [clojure.pprint :refer (cl-format print-table #_pprint #_pprint-str)])
  ;; NOTE/TODO: Puget seems like complete bullshit now; it doesn't care about flags set in .emacs or *print-length* or anything.
  ;;(:require [puget.printer :refer (cprint cprint-str) :rename {cprint pprint, cprint-str pprint-str}])
  (:require [zprint.core :refer (czprint czprint-str) :rename {czprint pprint, czprint-str pprint-str}])
  (:require [clojure.string :as str])
  (:require [clojure.core.async :as async])
  (:require [java-time :as jtime])
  (:require io.aviso.exception)
  (:require [net.cgrand.xforms.rfs :as rfs])
  (:require lrn-utils.misc)
  (:require lrn-utils.coll)
  (:require lrn-utils.unsync-mut)
  (:require lrn-utils.gist)
  (:require lrn-utils.debug)
  (:require lrn-utils.time))



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
  `first-request-blocking?`: If true (default is false), the first request (only) from `ch` is permitted to block."
  ([ch] (async-consume-buffer ch false))

  ([ch first-request-blocking?]
   (loop [e (if first-request-blocking?
              (async/<!! ch)
              (async/poll! ch))
          v (transient [])]
     (if e
       (recur (async/poll! ch) (conj! v e))
       (persistent! v)))))



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


(deftype GCedResource ;; For when you don't want to use WITH-OPEN and you're on drugs. Bonus: It will work in context of laziness.
    [resource]

  Object
  (finalize [o]
    #_(info "[GCedResource, finalize]:" resource)
    (.close resource)))



(defn double-mantissa "NOTE: Not accurate."
  ^double [^double x]
  (if (pos? x)
    (- x (Math/floor x))
    (- x (Math/ceil x))))



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



(defn file-atomic-move "Atomically move `src` file to `trg` file -- also replacing it if needed."
  [src trg]
  (let [src (java.nio.file.Paths/get (java.net.URI/create (str "file://" src)))
        trg (java.nio.file.Paths/get (java.net.URI/create (str "file://" trg)))]
    (java.nio.file.Files/move src trg (into-array java.nio.file.CopyOption
                                                  [(java.nio.file.StandardCopyOption/ATOMIC_MOVE)
                                                   (java.nio.file.StandardCopyOption/REPLACE_EXISTING)]))))
