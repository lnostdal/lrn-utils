(defproject lrn-utils "0.1.3"
  :description "A place where I dump common 'stuff' -- both bad, half-baked ideas and perhaps some good ones. Please don't use this, or if you do just extract ideas and snippets from it."
  :url "https://github.com/lnostdal/lrn-utils"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1-beta1"]
                 [org.clojure/core.async "LATEST"]
                 [net.cgrand/xforms "LATEST"]
                 ;;[zprint "LATEST"] ;; Alternate PPRINT facilities.
                 [mvxcvi/puget "LATEST"] ;; Alternate PPRINT facilities.
                 [io.aviso/pretty "LATEST"] ;; Pretty printer for exceptions.
                 [com.google.guava/guava "LATEST"]
                 [clojure.java-time "LATEST"]
                 [metosin/jsonista "LATEST"] ;; JSON parsing and generation.
                 [http-kit "LATEST"]
                 [com.taoensso/timbre "LATEST"]])

