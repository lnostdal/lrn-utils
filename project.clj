(defproject lrn-utils "0.1.0-SNAPSHOT"
  :description "A place where I dump common 'stuff' -- both bad, half-baked ideas and perhaps some good ones. Please don't use this, or if you do just extract ideas and snippets from it."
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "LATEST"]
                 [zprint "LATEST"] ;; Alternate PPRINT facilities.
                 [io.aviso/pretty "LATEST"] ;; Pretty printer for exceptions.
                 [com.google.guava/guava "LATEST"]
                 [clojure.java-time "LATEST"]]

  ;;:repl-options {:init-ns lrn-utils.core} ;; What sort of garbage idea or thing is this? x) This creates all kinds of problems when testing the lib solo.
  )
