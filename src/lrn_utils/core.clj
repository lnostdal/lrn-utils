(ns lrn-utils.core
  (:import (com.google.common.collect EvictingQueue)
           (com.google.common.cache Cache CacheBuilder CacheLoader))
  (:require [clojure.pprint :refer (cl-format print-table #_pprint #_pprint-str)]
            [puget.printer :refer (cprint cprint-str) :rename {cprint pprint, cprint-str pprint-str}]
            #_[zprint.core :refer (czprint czprint-str) :rename {czprint pprint, czprint-str pprint-str}]
            [clojure.string :as str]
            [clojure.core.async :as async]
            [java-time :as jtime]
            [io.aviso.exception]
            [net.cgrand.xforms.rfs :as rfs]
            [jsonista.core :as json]
            [clj-http.client]))


(require 'lrn-utils.misc)
(require 'lrn-utils.coll)
(require 'lrn-utils.unsync-mut)
(require 'lrn-utils.gist)
(require 'lrn-utils.debug)
(require 'lrn-utils.time)
