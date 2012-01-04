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
        [clojure.string :only [split split-lines]])
  (:import (java.io File)
           (javax.imageio ImageIO)))

;; Images used to represent circles of 2-8 arguments.
(def img-c2 (ImageIO/read (File. "images/nico_circ2.png")))
(def img-c3 (ImageIO/read (File. "images/nico_circ3.png")))
(def img-c4 (ImageIO/read (File. "images/nico_circ4.png")))
(def img-c5 (ImageIO/read (File. "images/nico_circ5.png")))
(def img-c6 (ImageIO/read (File. "images/nico_circ6.png")))
(def img-c7 (ImageIO/read (File. "images/nico_circ7.png")))
(def img-c8 (ImageIO/read (File. "images/nico_circ8.png")))

;; Width and height of the circle images.
(def circx (.. img-c2 getWidth))
(def circy (.. img-c2 getHeight))

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


(defmacro defcircle [name fun arg1 arg2 & args]
  "Creates a new circle represented by '(fun arg1 arg2 & args).  Can be nested."
  (let [xy (xy-rng)]
  `(def ~name
     {:x ~(:x xy)
      :y ~(:y xy)
      :img ~(symbol (str "img-c" (+ 2 (count args))))
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

;; (defcircle c0 + 1 2 3 4 5)
;; (defcircle c1 * 4 c0)
;; (defcircle c2 + 2 c1 4 c0 c1)

(defn string-to-int-list [s]
  "Takes a string of integers separated by spaces as returns a list of integers."
  (loop [in (split s #" ")
         out '()]
    (cond (empty? in) (reverse out)
          :else (recur (rest in) (cons (Integer/parseInt (first in)) out)))))

(defn new-circle []
  "Brings up a dialogue to define and draw a new circle on the Calculation canvas."
  (load-string (str "(defcircle c0 " (input "New:") ")")))

(defn edit-circle []
  "Brings up a dialogue to edit the parameters of an existing circle and redraws it."
    (load-string (str "(defcircle c0 " (input "Edit:") ")")))

(defn main-window []
  "Creates the contents of Nico's main window."
  (flow-panel :id :root
              :items [(canvas :id :canvas
                              :border "Calculation"
                              :size [640 :by 480])
                      (grid-panel :id :buttons
                                  :columns 1
                                  :items [(button :id :new
                                                  :text "New"
                                                  :listen [:mouse-clicked (fn [e] (new-circle))])
                                          (button :id :edit
                                                  :text "Edit"
                                                  :listen [:mouse-clicked (fn [e] (edit-circle))])])]))

(defn -main [& args]
  (do
    (native!)
    (invoke-later
     (-> (frame :title "Nico v0.0.1",
                :content (main-window),
                :on-close :exit)
         pack!
         show!))))