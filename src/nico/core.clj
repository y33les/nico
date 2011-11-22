(ns nico.core)

(defn read-qset [qsfile]
  "Reads a set of questions from the file qsfile into a list"
  (reverse
   (into ()
    (clojure.string/split-lines
     (slurp qsfile)))))


;; fix this, need recursive fn that gens e.g. (+ (* (- 3 2 1) 4) (+ 5 6)) for
;; several-deep circle maps
(defn circ-arg [circ]
  "Evaluates the expression represented by the circle circ"
  (cond (= (class circ) clojure.lang.PersistentArrayMap)
        (cons (:fn circ)
              (:args circ))
        :else circ))

(defn -main [& args]
  (prn "Hello, world!"))
