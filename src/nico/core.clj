(ns nico.core)

(defn read-qset [qsfile]
  "Reads a set of questions from the file qsfile into a list"
  (reverse
   (into ()
    (clojure.string/split-lines
     (slurp qsfile)))

(defn eval-circle [circ]
  "Evaluates the expression represented by the circle circ"
  (eval
   (cons (:fn circ)
         (:args circ))))

(defn -main [& args]
  (prn "Hello, world!"))
