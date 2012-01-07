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

(def used-circles
  ;; Agent listing symbols pointing to existing circles.
  (agent '()))

(defn read-qset [qsfile]
  "Reads a set of questions from the file qsfile into a list."
  (reverse
   (into ()
    (clojure.string/split-lines
     (slurp qsfile)))))

(defmacro defcircle [name fun arg1 arg2 & args]
  "Creates a new circle represented by '(fun arg1 arg2 & args) and adds a symbol pointing to it to used-circles.  Can be nested."
  `(do
     (def ~name
     {:x ~(+ 15 (. (java.util.Random.) nextInt 510))
      :y ~(+ 15 (. (java.util.Random.) nextInt 340))
      :name ~(str name)
      :circ (cons ~fun
                  (cons ~(cond (symbol? arg1) `(quote ~arg1)
                               :else arg1)
                        (cons ~(cond (symbol? arg2) `(quote ~arg2)
                                     :else arg2)
                              (quote ~args))))})
     (send-off used-circles #(cons (quote ~name) %))))

;; (def used-circles (agent '()))
;; (restart-agent used-circles '())

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
      (.setColor (java.awt.Color. 0x88FF88))
      (.fillOval x y 100 100)
      (.setColor java.awt.Color/BLACK)
      (.drawOval x y 100 100)
      (.setColor (java.awt.Color. 0x44BB44))
      (.fillOval (+ x 30) (+ y 30) 40 40)
      (.setColor java.awt.Color/BLACK)
      (.drawOval (+ x 30) (+ y 30) 40 40)
      (.drawString sym x (+ y 110))
      (.drawString op (+ x 50) (+ y 50))
      (.drawString (str args) x y)
      (.drawString (str @used-circles) (+ x 100) (+ y 100)))))

(defn clear-screen []
  "Clears all visible drawings from the canvas."
  (doto (.getGraphics (select main-window [:#canvas]))
    (.setColor java.awt.Color/WHITE)
    (.fillRect 15 15 610 450)))

(defn render []
  "Clears the screen, then draws all circles currently in used-circles."
  (do
    (clear-screen)
    (loop [u @used-circles]
      (cond (not (empty? u)) (do (draw-circle (first u)) (recur (rest u)))))))

(defn find-circle [name]
  "Returns the circle in used-circles with the :name property corresponding to name."
  (loop [n name
         u @used-circles]
    (cond (empty? u) (alert "Circle not found.")
          (= n (:name (first u))) (first u)
          :else (recur n (rest u)))))

;; (def used-circles (agent '()))
;; (restart-agent used-circles '())

(defn kill-used-circles []
  "Empties used-circles.  For use in debugging; should be removed from finished program."
  (def used-circles (agent '())))

(defn new-circle []
  "Brings up a dialogue to define and draw a new circle on the Calculation canvas."
  (let [in   (input "New:")
        name (first (split in #" "))
        fun  (eval (read-string (nth (split in #" ") 1)))
        expr (cons fun (rest (rest (read-string (str "(" in ")")))))
        circ {:x (+ 15 (. (java.util.Random.) nextInt 510))
              :y (+ 15 (. (java.util.Random.) nextInt 340))
              :name name
              :circ expr}]
                    ;; (cons fun
                    ;;      (cons ~(cond (not (integer? arg1)) `(quote ~arg1)
                    ;;                   :else arg1)
                    ;;            (cons ~(cond (not (integer? arg2)) `(quote ~arg2)
                    ;;                         :else arg2)
                    ;;                  (quote ~args))))}]
       (do
         (send-off used-circles #(cons circ %))
         (await used-circles)
         (draw-circle (find-circle name)))))
       ;; (load-string (str "(draw-circle " (first (split expr #" ")) ")"))
       ;; (load-string (str "(println (resolve '" (first (split expr #" ")) "))")))))


(defn del-circle [& name]
  "Removes a circle from used-circles, such that it won't reappear on executing render."
  (do
    (send-off used-circles (fn [_] (loop [in @used-circles
                                          out '()
                                          c (cond (nil? name) (input "Remove:")
                                                  :else (first name))]
                                     (cond (empty? in) (reverse out)
                                           (= (:name (first in)) c) (recur (rest in) out c)
                                           :else (recur (rest in) (cons (first in) out) c)))))
    ;; (await-for 2000 used-circles)
    (render)))

(defn edit-circle []
  "Brings up a dialogue to edit the parameters of an existing circle and redraws it."
  (let [in   (input "Edit:")
        name (first (split in #" "))
        fun  (eval (read-string (nth (split in #" ") 1)))
        expr (cons fun (rest (rest (read-string (str "(" in ")")))))
        old  (find-circle name)
        new  {:x (:x old)
              :y (:y old)
              :name (:name old)
              :circ expr}]
    (do
      (prn name)
      (prn expr)
      (prn (str "old: " old))
      (prn (str "new: " new))
      (del-circle name)
      (send-off used-circles #(cons new %))
      ;; (await used-circles)
      (render))))

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
                                                  :listen [:mouse-clicked (fn [e] (edit-circle))])
                                          (button :id :remove
                                                  :text "Remove"
                                                  :listen [:mouse-clicked (fn [e] (del-circle))])
                                          (button :id :render
                                                  :text "Render"
                                                  :listen [:mouse-clicked (fn [e] (render))])
                                          (button :id :clear
                                                  :text "Clear"
                                                  :listen [:mouse-clicked (fn [e] (clear-screen))])])])))

(defn -main [& args]
  (do
    (native!)
    (invoke-later
     (-> (frame :title "Nico v0.0.1",
                :content main-window,
                :on-close :exit)
         pack!
         show!))))