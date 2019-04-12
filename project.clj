(defproject lrn-utils "0.1.6"
  :description "A place where I dump common 'stuff' -- both bad, half-baked ideas and perhaps some good ones. Please don't use this, or if you do just extract ideas and snippets from it."
  :url "https://github.com/lnostdal/lrn-utils"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[javax.xml.bind/jaxb-api "2.3.1"] ;; Needed for newer JDKs.
                 [org.clojure/clojure "1.10.1-beta1"]
                 [org.clojure/core.async "0.4.490"]
                 [net.cgrand/xforms "0.19.0"] ;; Nice stuff for core.async.
                 ;;[zprint "LATEST"] ;; Alternate PPRINT facilities.
                 [mvxcvi/puget "1.1.2"] ;; Alternate PPRINT facilities.
                 [io.aviso/pretty "0.1.37"] ;; Pretty printer for exceptions.
                 [com.google.guava/guava "27.1-jre"]
                 [clojure.java-time "0.3.2"]
                 [metosin/jsonista "0.2.2"] ;; JSON parsing and generation.
                 [http-kit "2.4.0-alpha4"] ;; HTTP server, client and websocket stuff.
                 [com.taoensso/timbre "4.10.0"]])
