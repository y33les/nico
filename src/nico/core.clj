(ns nico.core
  (:gen-class)
  (:use (guiftw swt styles)
        [clojure.string :only [split-lines]])
  (:import (org.eclipse.swt SWT)
           (org.eclipse.swt.widgets Shell Button MessageBox Canvas)
           (org.eclipse.swt.events SelectionListener)
           (org.eclipse.swt.layout GridLayout GridData)))

(defn read-qset [qsfile]
  "Reads a set of questions from the file qsfile into a list."
  (reverse
   (into ()
    (clojure.string/split-lines
     (slurp qsfile)))))

;; (defn agent? [a]
;;   "Returns true if a is an agent."
;;   (= (class a) clojure.lang.Agent))

;; (defn make-vals [arg-list]
;;   "Takes a list of ints and agents and returns the same list with agents derefed."
;;   (loop [l arg-list
;;          out '()]
;;     (cond (empty? l) (reverse out)
;;           (agent? (eval (first l))) (recur (rest l) (cons (deref (eval (first l))) out))
;;           :else (recur (rest l) (cons (first l) out)))))

;; (defmacro defcircle [name fun arg1 arg2 & args]
;;   "Creates a new agent name representing a circle containing (fun arg1 arg2 & args)."
;;   `(def ~name
;;      (agent
;;       (cons ~fun
;;             (cons ~(cond (agent? (eval arg1)) `(eval (deref (eval ~arg1)))
;;                         :else arg1)
;;                   (cons ~(cond (agent? (eval arg2)) `(eval (deref (eval ~arg2)))
;;                               :else arg2)
;;                         (quote ~(make-vals args))))))))

(defmacro defcircle [name fun arg1 arg2 & args]
  "Creates a new circle represented by '(fun arg1 arg2 & args).  Can be nested."
  `(def ~name
     (cons ~fun
           (cons ~(cond (symbol? arg1) `(quote ~arg1)
                        :else arg1)
                 (cons ~(cond (symbol? arg2) `(quote ~arg2)
                              :else arg2)
                       (quote ~args))))))

(defn nested? [circ]
  "Returns true if a circle contains other circles."
  (cond (not (= (class circ) clojure.lang.Cons)) false
        :else (loop [c circ]
                (cond (empty? c) false
                      (= (class (eval (first c))) clojure.lang.Cons) true
                      :else (recur (rest c))))))

(defn eval-circle [circ]
  "Iterates across a circle list, resolving symbols into their respective circles."
  (loop [c circ
         out '()]
    (cond (empty? c) (reverse out)
          (nested? (eval (first c))) (recur (rest c) (cons (eval-circle (eval (first c))) out))
          :else (recur (rest c) (cons (eval (first c)) out)))))

(def window
  "Defines the contents of the main application window."
  (swt
   [Shell [*id :main-window]
    [Canvas [*id :circle-area]]
    [Button [*id :new-circle]]
    [Button [*id :split-circle]]
    [Button [*id :del-circle]]]))

(def look
  "Defines button behaviour and the style of the main application window."
  (stylesheet
   [:main-window] [:text "Nico v0.0.1"
                   :size ^unroll (640 480)
                   :layout (GridLayout. 3 true)]
   [:circle-area] [:*cons [SWT/NONE]
                   :layoutData (GridData. GridData/FILL GridData/BEGINNING true true 3 1)]
   [:new-circle] [:*cons [SWT/PUSH]
                  :text "New"]
   [:split-circle] [:*cons [SWT/PUSH]
                    :text "Split"]
   [:del-circle] [:*cons [SWT/PUSH]
                  :text "Delete"]))

(defn new-circle [gui event]
  "Creates a new circle agent and displays it onscreen."
  (doto (MessageBox.
         (:root @gui)
         (bit-or SWT/ICON_INFORMATION SWT/OK))
    (.setText "New Circle")
    (.setMessage "New!")
    .open))

(defn split-circle [gui event]
  "Increments the number of arguments to a circle by 1 and displays this onscreen."
  (doto (MessageBox.
         (:root @gui)
         (bit-or SWT/ICON_INFORMATION SWT/OK))
    (.setText "Split Circle")
    (.setMessage "Split!")
    .open))

(defn del-circle [gui event]
  "Removes a circle from view."
  (doto (MessageBox.
         (:root @gui)
         (bit-or SWT/ICON_INFORMATION SWT/OK))
    (.setText "Delete Circle")
    (.setMessage "Delete!")
    .open))

(def actions
  "Defines button behaviour."
  (stylesheet
   [:new-circle] [:selection+widget-selected new-circle]
   [:split-circle] [:selection+widget-selected split-circle]
   [:del-circle] [:selection+widget-selected del-circle]))

(defn -main [& args]
  (let [gui (window look actions)
        shell (:root @gui)]
    (.open shell)
    (swt-loop shell)))