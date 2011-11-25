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

;; fn to parse arguments to check if circle and deconstruct as appropriate (BROKEN)
(defn parse-args [args]
  "Parses the output of circ-arg to deconstruct circles"
  (loop [out '()
         args a]
    (cond (not (nil? a))
          (cond (= (class (first a) clojure.lang.PersistenArrayMap))
                (cons (recur (circ-arg (first a))) out)
                :else (cons (first a) out))
          (recur (rest a)))))

(defn -main [& args]
  (prn "Hello, world!"))
