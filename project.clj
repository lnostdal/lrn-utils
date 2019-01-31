(defproject lrn-utils "0.1.0-SNAPSHOT"
  :description "A place where I dump common 'stuff' -- both bad, half-baked ideas and perhaps some good ones."
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "LATEST"]
                 [org.clojure/core.async "LATEST"]
                 [zprint "LATEST"] ;; Alternate PPRINT facilities.
                 [io.aviso/pretty "LATEST"] ;; Pretty printer for exceptions.
                 [com.google.guava/guava "LATEST"]
                 [clj-time/clj-time "LATEST"]] ;; TODO: Switch to https://github.com/dm3/clojure.java-time (below).
  :repl-options {:init-ns lrn-utils.core})
