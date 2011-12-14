;; Nico: An Environment for Mathematical Expression in Schools
;; Copyright (C) 2011  Philip M. Yeeles
;; 
;; This file is part of Nico.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns nico.core
  (:gen-class)
  (:use (guiftw swt styles)
        [clojure.string :only [split-lines]])
  (:import (org.eclipse.swt SWT)
           (org.eclipse.swt.widgets Shell Button MessageBox Canvas Display)
           (org.eclipse.swt.events SelectionListener PaintListener PaintEvent)
           (org.eclipse.swt.layout GridLayout GridData)
           (org.eclipse.swt.graphics GC Image Device Rectangle Point)))

(defn init-repl []
  (do
    (import (org.eclipse.swt SWT))
    (import (org.eclipse.swt.widgets Shell Button MessageBox Canvas Display))
    (import (org.eclipse.swt.events SelectionListener PaintListener PaintEvent))
    (import (org.eclipse.swt.layout GridLayout GridData))
    (import (org.eclipse.swt.graphics GC Image Device Rectangle Point))
    (def img-c2 (Image. (Display/getCurrent) "images/nico_circ2.png"))
    (def img-c3 (Image. (Display/getCurrent) "images/nico_circ3.png"))
    (def img-c4 (Image. (Display/getCurrent) "images/nico_circ4.png"))
    (def img-c5 (Image. (Display/getCurrent) "images/nico_circ5.png"))
    (def img-c6 (Image. (Display/getCurrent) "images/nico_circ6.png"))
    (def img-c7 (Image. (Display/getCurrent) "images/nico_circ7.png"))
    (def img-c8 (Image. (Display/getCurrent) "images/nico_circ8.png"))))

;;(init-repl)

;; Images used to represent circles of 2-8 arguments.
(def img-c2 (Image. (Display/getCurrent) "images/nico_circ2.png"))
(def img-c3 (Image. (Display/getCurrent) "images/nico_circ3.png"))
(def img-c4 (Image. (Display/getCurrent) "images/nico_circ4.png"))
(def img-c5 (Image. (Display/getCurrent) "images/nico_circ5.png"))
(def img-c6 (Image. (Display/getCurrent) "images/nico_circ6.png"))
(def img-c7 (Image. (Display/getCurrent) "images/nico_circ7.png"))
(def img-c8 (Image. (Display/getCurrent) "images/nico_circ8.png"))

;; Width and height of the circle images.
(def circx (.. img-c2 getBounds width))
(def circy (.. img-c2 getBounds height))

(def used-coords
  ;; Agent listing locations of existing circles.
  (agent '()))

(defn read-qset [qsfile]
  "Reads a set of questions from the file qsfile into a list."
  (reverse
   (into ()
    (clojure.string/split-lines
     (slurp qsfile)))))

(defn xy-rng []
  (let [x (. (java.util.Random.) nextInt 640) ;; canvx
        y (. (java.util.Random.) nextInt 480)] ;; canvy
    (do
      (send-off used-coords #(cons (list x y) %))
      {:x x
       :y y})))

(defmacro defcircle-map [name fun arg1 arg2 & args]
  "Creates a new circle represented by '(fun arg1 arg2 & args).  Can be nested."
  `(def ~name
     {:x ~(. (java.util.Random.) nextInt 640) ;; canvx)
      :y ~(. (java.util.Random.) nextInt 480) ;; canvy)
      :img-circ ~(symbol (str "img-c" (+ 2 (count args))))
      :circ (cons ~fun
                  (cons ~(cond (symbol? arg1) `(quote ~arg1)
                               :else arg1)
                        (cons ~(cond (symbol? arg2) `(quote ~arg2)
                                     :else arg2)
                              (quote ~args))))}))

(defn defcircle [name fun arg1 arg2 & args]
  "Creates a new circle using defcircle-map and adds a list of its x and y co-ordinates to used-coords."
  (do
    (loop [in args
           out (list arg2 arg1 fun name 'defcircle-map)]
      (cond (empty? in) (do (prn (reverse out)) (eval (reverse out)))
            :else (recur (rest in) (cons (first in) out))))
    (send-off used-coords #(cons '((:x name) (:y name)) %))))

(defn nested? [circ]
  "Returns true if a circle contains other circles."
  (cond (not (= (class circ) clojure.lang.Cons)) false
        :else (loop [c circ]
                (cond (empty? c) false
                      (= (class (eval (first c))) clojure.lang.PersistentArrayMap) true
                                            :else (recur (rest c))))))

(defn eval-circle [circ]
  "Iterates across a circle list, resolving symbols into their respective circles."
  (loop [c (:circ circ)
         out '()]
    (cond (empty? c) (reverse out)
          (map? (eval (first c))) (cond (nested? (:circ (eval (first c)))) (recur (rest c) (cons (eval-circle (eval (first c))) out))
                                        :else (recur (rest c) (cons (:circ (eval (first c))) out)))
          :else (recur (rest c) (cons (eval (first c)) out)))))

;; (defcircle c0 + 1 2 3 4 5)
;; (defcircle c1 * 4 c0)
;; (defcircle c2 + 2 c1 4 c0 c1)

(def window
  ;; Defines the contents of the main application window.
  (swt
   [Shell [*id :main-window]
    [Canvas [*id :circle-area]]
    [Button [*id :new-circ]]
    [Button [*id :split-circ]]
    [Button [*id :del-circ]]]))

(def look
  ;; Defines button behaviour and the style of the main application window.
  (stylesheet
   [:main-window] [:text "Nico v0.0.1"
                   :size ^unroll (640 480)
                   :layout (GridLayout. 3 true)]
   [:circle-area] [:*cons [SWT/NONE]
                   :layoutData (GridData. GridData/FILL GridData/BEGINNING true true 3 1)]
   [:new-circ] [:*cons [SWT/PUSH]
                :text "New"]
   [:split-circ] [:*cons [SWT/PUSH]
                  :text "Split"]
   [:del-circ] [:*cons [SWT/PUSH]
                :text "Delete"]))

;; Width and height of the canvas.
(def canvx (. (-> @gui :main-window :circle-area) getSize) x)
(def canvy (. (-> @gui :main-window :circle-area) getSize) y)

;; (defn new-circle [gui event]
;;   "Creates a new circle agent and displays it onscreen."
;;   (doto (MessageBox.
;;          (:root @gui)
;;          (bit-or SWT/ICON_INFORMATION SWT/OK))
;;     (.setText "New Circle")
;;     (.setMessage "New!")
;;     .open))

(defn get-circ-bounds [circ]
  "Returns a map of the x- and y-co-ordinates and the height and width of the bounding box around a circle image."
  (

(defn new-circle [circ gui event]
  "Displays a new circle onscreen."
  (do
    (doto (-> @gui :main-window :circle-area)
      ())
    (send-off used-coords #(cons circ %))))

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
  ;; Defines button behaviour.
  (stylesheet
   [:new-circ] [:selection+widget-selected new-circle]
   [:split-circ] [:selection+widget-selected split-circle]
   [:del-circ] [:selection+widget-selected del-circle]))

(defn -main [& args]
  (let [gui (window look actions)
        shell (:root @gui)]
    (.open shell)
    (swt-loop shell)))