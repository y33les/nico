;; Nico: An Environment for Mathematical Expression in Schools
;; Copyright (C) 2011-2012  Philip M. Yeeles
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
  (:use (seesaw core graphics)
        [clojure.string :only [split split-lines]]))

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
  (let [x (+ 15 (. (java.util.Random.) nextInt 510))
        y (+ 15 (. (java.util.Random.) nextInt 350))]
    (do
      (send-off used-coords #(cons (list x y) %))
      {:x x
       :y y})))


(defmacro defcircle [name fun arg1 arg2 & args]
  "Creates a new circle represented by '(fun arg1 arg2 & args).  Can be nested."
  (let [xy (xy-rng)]
  `(def ~name
     {:x ~(:x xy)
      :y ~(:y xy)
      :name ~(str name)
      :circ (cons ~fun
                  (cons ~(cond (symbol? arg1) `(quote ~arg1)
                               :else arg1)
                        (cons ~(cond (symbol? arg2) `(quote ~arg2)
                                     :else arg2)
                              (quote ~args))))})))

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

(defn string-to-int-list [s]
  "Takes a string of integers separated by spaces as returns a list of integers."
  (loop [in (split s #" ")
         out '()]
    (cond (empty? in) (reverse out)
          :else (recur (rest in) (cons (Integer/parseInt (first in)) out)))))

(def main-window)

(defn draw-circle [circ]
  "Draws a circle circ at co-ordinates (x,y) given a canvas c and a Graphics2D g."
  (let [g (.getGraphics (select main-window [:#canvas]))
        x (:x circ)
        y (:y circ)
        op (cond (= (first (:circ circ)) +) "+"
                 (= (first (:circ circ)) -) "-"
                 (= (first (:circ circ)) *) "*"
                 (= (first (:circ circ)) /) "/"
                 :else "error")
        args (rest (:circ circ))
        sym (:name circ)]
    (doto g
      (.setColor java.awt.Color/BLACK)
      (.setBackground java.awt.Color/RED)
      (.drawOval x y 100 100)
      (.drawOval (+ x 30) (+ y 30) 40 40)
      (.drawString sym x (+ y 110))
      (.drawString op (+ x 50) (+ y 50)))))
      
(defn clear-screen []
  "Clears all visible drawings from the canvas."
  (doto (.getGraphics (select main-window [:#canvas]))
    (.setColor java.awt.Color/WHITE)
    (.fillRect 15 15 610 450)))

(defn new-circle []
  "Brings up a dialogue to define and draw a new circle on the Calculation canvas."
  (let [expr (read-string (str "(defcircle " (input "New:") ")"))]
  (do
    (load-string (str "(defcircle " (input "New:") ")")))))
    ;; (draw-circle (select main-window [:#canvas]) (.getGraphics (select main-window [:#canvas])) c0 x y))))

(defn edit-circle []
  "Brings up a dialogue to edit the parameters of an existing circle and redraws it."
  (load-string (str "(defcircle " (input "Edit:") ")")))

(def main-window
  ;; Creates the contents of Nico's main window.
  (do (native!)
  (flow-panel :id :root
              :items [(canvas :id :canvas
                              :background "#FFFFFF"
                              :border "Calculation"
                              :size [640 :by 480])
                      (grid-panel :id :buttons
                                  :columns 1
                                  :items [(button :id :new
                                                  :text "New"
                                                  :listen [:mouse-clicked (fn [e] (new-circle))])
                                          (button :id :edit
                                                  :text "Edit"
                                                  :listen [:mouse-clicked (fn [e] (edit-circle))])])])))

(defn -main [& args]
  (do
    (native!)
    (invoke-later
     (-> (frame :title "Nico v0.0.1",
                :content main-window,
                :on-close :exit)
         pack!
         show!))))