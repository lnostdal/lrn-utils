(in-ns 'lrn-utils.core)

;; Try to make java.time.* and the clojure.java-time library less annoying to use
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   * (clj-time.core/now) is now (jtime/instant)
;;   * (clj-time.coerce/to-long (time/now)) => (ts-to-long (jtime/instant)
;;   * (clj-time/seconds 30) => (jtime/duration 30 :seconds)
;;   * (clj-time/in-seconds (clj-time/interval (clj-time/now) (clj-time/seconds 5))) =>
;;       (jtime/as (jtime/duration (jtime/local-date-time)
;;                                 (jtime/plus (jtime/local-date-time) (jtime/duration 5 :seconds)))
;;                 :seconds)
;;  * (clj-time/floor .. clj-time/day) => (jtime/truncate-to .. :days)

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
    String (try ;; NOTE: This is crazy slow when parsing fails; use explicit format instead!
             (to-ts (jtime/zoned-date-time i)) ;; E.g. "2019-02-11T18:47:58Z"
             (catch clojure.lang.ExceptionInfo e
               (if (instance? java.time.DateTimeException (ex-cause e))
                 ;; E.g. "2018-01-01". NOTE: "2018-1-1" won't work!
                 (try (to-ts (jtime/truncate-to (jtime/local-date-time (jtime/local-date i)) :days))
                      (catch clojure.lang.ExceptionInfo e
                        ;; TODO: Pretty horrid, but seem necessary for this exception to be visible in Cider atm..
                        (throw (ex-cause e))))
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
