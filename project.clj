(defproject nico "0.0.1-SNAPSHOT"
  :description "An Environment for Mathematical Expression in Schools"
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]]
  :repl-options [:init nil :caught clj-stacktrace.repl/pst+]
  :main nico.core)
