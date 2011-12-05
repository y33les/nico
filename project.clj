(defproject nico "0.0.1-SNAPSHOT"
  :description "An Environment for Mathematical Expression in Schools"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [guiftw "0.2.0-SNAPSHOT"]
                 [org.eclipse/swt-gtk-linux-x86 "3.5.2"]]
  :dev-dependencies [[swank-clojure "1.2.0-SNAPSHOT"]]
  :repl-options [:init nil :caught clj-stacktrace.repl/pst+]
  :main nico.core)
