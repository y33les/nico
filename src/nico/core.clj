(ns nico.core
  (:gen-class)
  (:use (guiftw swt styles)
        [clojure.string :only [split-lines]])
  (:import (org.eclipse.swt SWT)
           (org.eclipse.swt.widgets Shell Button MessageBox Canvas)
           (org.eclipse.swt.events SelectionListener)
           (org.eclipse.swt.layout RowLayout)))

(defn read-qset [qsfile]
  "Reads a set of questions from the file qsfile into a list"
  (reverse
   (into ()
    (clojure.string/split-lines
     (slurp qsfile)))))

(def window
  (swt
   [Shell [*id :main-window]
    [Canvas [*id :circle-area]]]))

(def look
  (stylesheet
   [:main-window] [:text "Nico v0.0.1"
                   :size ^unroll (640 480)
                   :layout (RowLayout.)]
   [:circle-area] [:*cons [SWT/NONE]]))

(def actions '())

(defn -main [& args]
  (let [gui (window look actions)
        shell (:root @gui)]
    (.open shell)
    (swt-loop shell)))
