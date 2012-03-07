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
  (:use [seesaw.core]
        [seesaw.graphics]
        [seesaw.chooser]
        [clojure.string :only [split split-lines escape]])
  (:import (java.awt RenderingHints)))

(def screen-x
  ;; Available screen width.
  800) ;; (int (.getWidth (.getScreenSize (java.awt.Toolkit/getDefaultToolkit)))))

(def screen-y
  ;; Available screen height.
  600) ;;(int (.getHeight (.getScreenSize (java.awt.Toolkit/getDefaultToolkit)))))

(def used-circles
  ;; Agent listing symbols pointing to existing circles.
  (agent '()))

(def current-qset
  ;; Agent listing question set read from a file.
  (agent '()))

(def current-question
  ;; Agent containing an integer corresponding to the quesiton number.
  (agent 0))

(def current-coords
  ;; Agent containing a map of the last co-ordinates the mouse was at.
  (agent {:x 0 :y 0}))

(def currently-dragging-circle
  ;; Agent containing the name of the circle that is currently being dragged.
  (agent nil))

(defn kill-used-circles
  "Empties used-circles.  For use in debugging; should be removed from finished program."
  []
  (def used-circles (agent '())))

(defn kill-current-qset
  "Empties current-qset.  For use in debugging; should be removed from finished program."
  []
  (def current-qset (agent '())))

(defn lisp-to-maths
  "Translates a sexp into a string of child-readable maths."
  [sexp]
  (loop [s   (rest sexp)
         op  (cond (= (eval (first sexp)) +) "+"
                   (= (eval (first sexp)) -) "-"
                   (= (eval (first sexp)) *) (str \u00d7)
                   (= (eval (first sexp)) /) (str \u00f7)
                   :else "error")
         out ""
         n 0]
    (cond (empty? s) (str out "")
          (= (first sexp) \P) \P
          (odd? n) (recur s op (str out op) (inc n))
          (list? (first s)) (recur (rest s) op (str out "(" (lisp-to-maths (first s)) ")") (inc n))
          :else (recur (rest s) op (str out (first s)) (inc n)))))

(defn string-to-panel
  "Breaks the string s down into a series of labels, one per character, contained within a horizontal-panel."
  [s]
  (let [p (horizontal-panel :id :question
                            :background "#FFFFFF")]
    (loop [q s
           i []
           n 0]
      (cond (empty? q) (do
                         (config! p :items i)
                         p)
            :else (recur (rest q)
                         (conj i (label :id (keyword (str "l" n))
                                        :text (str (first q))
                                        :font {:name :sans-serif :style :bold :size 18}
                                        :foreground "#000000"))
                         (inc n))))))

(defn test-labels
  "Test question-labels."
  []
  (let [h (string-to-panel (lisp-to-maths (eval (:q (first @current-qset)))))
        f (frame :title "lol"
                 :content h
                 :on-close :dispose)]
    (-> f pack! show!)))

;; (kill-current-qset)
;; (test-labels)

(defn find-circle
  "Returns the circle in used-circles with the :name property corresponding to name."
  [name]
  (loop [n name
         u @used-circles]
    (cond (empty? u) nil ;; (alert "Circle not found.")
          (= n (:name (first u))) (first u)
          :else (recur n (rest u)))))

(defn read-qset
  "Reads a set of questions from the file qsfile into a list of maps."
  [qsfile]
  (loop [in (reverse (into () (clojure.string/split-lines (slurp qsfile))))
         out '()
         n 1]
    (cond (empty? in) (reverse out)
          :else (recur
                 (rest in)
                 (cons
                  (read-string (str
                                "{:n "
                                n
                                " :q '"
                                (first in)
                                " :a? false :c? false}"))
                  out)
                 (inc n)))))

(defn get-q-no
  "Gets the question from the current set with the :n field of value num."
  [num]
  (loop [qs @current-qset
         n  num]
    (cond (empty? qs) nil
          (= n (:n (first qs))) (first qs)
          :else (recur (rest qs) n))))

(defn nested?
  "Returns true if a circle contains other circles."
  [circ]
  (cond (not (map? circ)) false
        :else (loop [c (:circ circ)]
                (cond (empty? c) false
                      (symbol? (first c)) true
                      :else (recur (rest c))))))

(defn has-placeholders?
  "Returns true if a circle circ contains any placeholders (operator or arguments)."
  [circ]
  (loop [c (:circ circ)]
    (cond (empty? c) false
          (= (first c) \P) true
          :else (recur (rest c)))))

(defn remove-placeholders
  "Returns a circle c with its placeholder arguments removed."
  [c]
  {:x (:x c)
   :y (:y c)
   :name (:name c)
   :circ (loop [in (rest (:circ c))
                out (list (first (:circ c)))]
           (cond (empty? in) (reverse out)
                 (= \P (first in)) (recur (rest in) out)
                 :else (recur (rest in) (cons (first in) out))))})

(defn eval-circle
  "Iterates across a circle list, resolving symbols into their respective circles."
  [circ]
  (loop [c (:circ (remove-placeholders circ))
         out '()]
    (cond (empty? c) (reverse out)
          (= (first c) \P) 0
          (symbol? (first c)) (cond (nested? (find-circle (str (first c)))) (recur (rest c) (cons (eval-circle (find-circle (str (first c)))) out))
                                    :else (recur (rest c) (cons (:circ (find-circle (str (first c)))) out)))
          :else (recur (rest c) (cons (first c) out)))))

(def main-window)

(defn in-circle
  "Checks if the supplied co-ordinate is within a circle (checks against :x if x? is true, :y otherwise) and, if so, returns the name of that circle as a string (otherwise returns nil)."
  [x x?]
  (loop [u @used-circles]
    (cond x? (cond (empty? u) nil
                   (and (> x (:x (first u)))
                        (< x (+ (:x (first u)) 100))) (:name (first u))
                   :else (recur (rest u)))
          :else (cond (empty? u) nil
                      (and (> x (:y (first u)))
                           (< x (+ (:y (first u)) 100))) (:name (first u))
                      :else (recur (rest u))))))

(defn in-circle?
  "Wrapper function for in-circle that returns true if x is within the horizontal (if x? is true, vertical if x? is false) range of any circle and false otherwise."
  [x x?]
  (not (nil? (in-circle x x?))))

(defn point-in-circle
  "Checks if the supplied co-ordinates are within a circle and, if so, returns the name of that circle (else nil)."
  [x y]
  (loop [u @used-circles]
    (cond (empty? u) nil
          (and (> x (:x (first u)))
               (< x (+ (:x (first u)) 100))
               (> y (:y (first u)))
               (< y (+ (:y (first u)) 100))) (:name (first u))
          :else (recur (rest u)))))

(defn arg-in-circle
  "Checks if the supplied co-ordinates correspond to an argument within a circle and, if so, return the index of that argument."
  [x y]
  (let [c  (point-in-circle x y)
        c? (not (nil? c))
        a  (rest (:circ (find-circle c)))
        xs (cond (= (count a) 2) '(48 48)
                 (= (count a) 3) '(48 73 23)
                 (= (count a) 4) '(48 83 48 13)
                 (= (count a) 5) '(48 80 68 28 16)
                 (= (count a) 6) '(48 79 79 48 17 17)
                 (= (count a) 7) '(48 76 78 64 32 18 20)
                 (= (count a) 8) '(48 73 83 73 48 23 13 23))
        ys (cond (= (count a) 2) '(21 89)
                 (= (count a) 3) '(21 80 80)
                 (= (count a) 4) '(21 55 89 55)
                 (= (count a) 5) '(21 43 82 82 43)
                 (= (count a) 6) '(21 40 72 89 72 40)
                 (= (count a) 7) '(21 37 66 86 86 66 37)
                 (= (count a) 8) '(21 32 55 80 89 80 55 32))
        cx (:x (find-circle c))
        cy (:y (find-circle c))]
    (cond c? (loop [lx xs
                    ly ys
                    n 0]
               (cond (empty? lx) nil
                     (and (>= x (+ cx (first lx)))
                          (< x (+ cx (first lx) 16))
                          (>= y (- (+ cy (first ly)) 16))
                          (< y (+ cy (first ly)))) n
                     :else (recur (rest lx) (rest ly) (inc n)))))))

(defn op-in-circle?
  "Returns true if the supplied co-ordinates are over the operator of a circle."
  [x y]
  (let [c  (point-in-circle x y)
        c? (not (nil? c))
        cx (:x (find-circle c))
        cy (:y (find-circle c))]
    (cond (and c?
               (>= x (+ cx 46))
               (< x (+ cx 46 16))
               (>= y (- (+ cy 54) 16))
               (< y (+ cy 54))) true
          :else false)))

(defn available?
  "Wrapper function for in-circle? that also checks if the generated co-ordinate will be out of bounds.  If x? is true it checks a x co-ordinate, if not it checks a y co-ordinate."
  [x x?]
  (cond x? (not (or (in-circle? x x?) (> (+ x 100) screen-x) (in-circle (+ x 100) x?)))
        :else (not (or (in-circle? x x?) (> (+ x 100) screen-y) (in-circle (+ x 100) x?)))))

(comment
(defn count-nested
  "Returns an integer corresponding to how many nests of circles a given circle contains."
  [circ]
  (loop [c (rest (:circ circ))
         n 1]
    (cond (empty? c) n
          ;; (symbol? (first c)) (cond (nested? (find-circle (str (first c)))) (recur (rest c) (+ n (count-nested (find-circle (str (first c))))))
          ;;                           :else (recur (rest c) (inc n)))
          (symbol? (first c)) (recur (rest c) (+ n (count-nested (find-circle (str (first c))))))
          :else (recur (rest c) n))))
)

(defn leaf?
  "Returns true if the circle does not have any other circles as arguments."
  [circ]
  (loop [c (rest (:circ circ))
         l true]
    (cond (empty? c) l
          (symbol? (first c)) false
          :else (recur (rest c) l))))

(defn nested-circles
  "Returns a list of circ's arguments that are other circles."
  [circ]
  (loop [in  (rest (:circ circ))
         out '()]
    (cond (empty? in) (reverse out)
          (symbol? (first in)) (recur (rest in) (cons (str (first in)) out))
          :else (recur (rest in) out))))

(defn is-arg?
  "Returns true if circ1 is an argument of circ2."
  [circ1 circ2]
  (loop [n (:name circ1)
         c (rest (:circ circ2))
         a false]
    (cond (empty? c) a
          (= n (str (first c))) true
          :else (recur n (rest c) a))))

(defn root?
  "Returns true if circ is not used as an argument to any other circle."
  [circ]
  (loop [c circ
         u @used-circles
         r true]
    (cond (empty? u) r
          (is-arg? c (first u)) false
          :else (recur c (rest u) r))))

(defn find-root
  "Returns the root circle (i.e. the circle to be evaluated that contains all others)."
  []
  (loop [u @used-circles
         c nil]
    (cond (empty? u) c
          (root? (first u)) (first u)
          :else (recur (rest u) c))))

(defn find-longest-path
  "Find the longest path to the circle c."
  [c]
  [])

(defn count-nested
  "Returns an integer representing the number of levels traversed before hitting a root node."
  [circ]
  (loop [c (rest (:circ circ))
         ;; s is number of symbols in current circle
         s (loop [cp c
                  n 0]
             (cond (empty? cp) n
                   (symbol? (first cp)) (recur (rest cp) (inc n))
                   :else (recur (rest cp) n)))
         n 1]
    (cond (empty? c) n
          (symbol? (first c)) (cond (> s 1) (recur (rest c) s (+ n (count-nested (find-circle (find-longest-path c)))))
                                    :else (recur (rest c) s (+ n (count-nested (find-circle (str (first c))))))
          :else (recur (rest c) s n)))))

(defn link-circles
  "Draws a lines from a nested circle to its circle-valued arguments."
  [circ]
  (let [x  (:x circ)
        y  (:y circ)
        cr (rest (:circ circ))
        xs (cond (= (count cr) 2) '(48 48)
                 (= (count cr) 3) '(48 73 23)
                 (= (count cr) 4) '(48 83 48 13)
                 (= (count cr) 5) '(48 80 68 28 16)
                 (= (count cr) 6) '(48 79 79 48 17 17)
                 (= (count cr) 7) '(48 76 78 64 32 18 20)
                 (= (count cr) 8) '(48 73 83 73 48 23 13 23))
        ys (cond (= (count cr) 2) '(21 89)
                 (= (count cr) 3) '(21 80 80)
                 (= (count cr) 4) '(21 55 89 55)
                 (= (count cr) 5) '(21 43 82 82 43)
                 (= (count cr) 6) '(21 40 72 89 72 40)
                 (= (count cr) 7) '(21 37 66 86 86 66 37)
                 (= (count cr) 8) '(21 32 55 80 89 80 55 32))]
    (loop [c cr
           n 0]
           ;; x (+ (:x circ) 50)
           ;; y (+ (:y circ) 50)]
      (cond (empty? c) nil
            (symbol? (first c)) (do
                                  (let [t  (find-circle (str (first c)))
                                        ox (+ x (nth xs n) 4)
                                        oy (+ y (nth ys n) -4)
                                        tx (+ (:x t) 50)
                                        ty (+ (:y t) 50)]
                                    (doto (.getGraphics (select main-window [:#canvas]))
                                      (.setColor java.awt.Color/BLACK)
                                      (.drawLine ox oy tx ty)
                                      (.drawOval (- ox 8) (- oy 8) 16 16)))
                                  (recur (rest c) (inc n)))
            :else (recur (rest c) (inc n))))))

(defn draw-args
  "Draws the arguments of an expression around its circle."
  [g a x y]
  (let [args (loop [in  a
                    out '()]
               (cond (empty? in) (reverse out)
                     (symbol? (first in)) (recur (rest in) (cons " " out))
                     :else (recur (rest in) (cons (first in) out))))]
    (cond (= (count args) 2) (doto g
                               (.drawString (str (nth args 0)) (+ x 48) (+ y 21))
                               (.drawString (str (nth args 1)) (+ x 48) (+ y 89)))
          (= (count args) 3) (doto g
                               (.drawString (str (nth args 0)) (+ x 48) (+ y 21))
                               (.drawString (str (nth args 1)) (+ x 73) (+ y 80))
                               (.drawString (str (nth args 2)) (+ x 23) (+ y 80)))
          (= (count args) 4) (doto g
                               (.drawString (str (nth args 0)) (+ x 48) (+ y 21))
                               (.drawString (str (nth args 1)) (+ x 83) (+ y 55))
                               (.drawString (str (nth args 2)) (+ x 48) (+ y 89))
                               (.drawString (str (nth args 3)) (+ x 13) (+ y 55)))
          (= (count args) 5) (doto g
                               (.drawString (str (nth args 0)) (+ x 48) (+ y 21))
                               (.drawString (str (nth args 1)) (+ x 80) (+ y 43))
                               (.drawString (str (nth args 2)) (+ x 68) (+ y 82))
                               (.drawString (str (nth args 3)) (+ x 28) (+ y 82))
                               (.drawString (str (nth args 4)) (+ x 16) (+ y 43)))
          (= (count args) 6) (doto g
                               (.drawString (str (nth args 0)) (+ x 48) (+ y 21))
                               (.drawString (str (nth args 1)) (+ x 79) (+ y 40))
                               (.drawString (str (nth args 2)) (+ x 79) (+ y 72))
                               (.drawString (str (nth args 3)) (+ x 48) (+ y 89))
                               (.drawString (str (nth args 4)) (+ x 17) (+ y 72))
                               (.drawString (str (nth args 5)) (+ x 17) (+ y 40)))
          (= (count args) 7) (doto g
                               (.drawString (str (nth args 0)) (+ x 48) (+ y 21))
                               (.drawString (str (nth args 1)) (+ x 76) (+ y 37))
                               (.drawString (str (nth args 2)) (+ x 78) (+ y 66))
                               (.drawString (str (nth args 3)) (+ x 64) (+ y 86))
                               (.drawString (str (nth args 4)) (+ x 32) (+ y 86))
                               (.drawString (str (nth args 5)) (+ x 18) (+ y 66))
                               (.drawString (str (nth args 6)) (+ x 20) (+ y 37)))
          (= (count args) 8) (doto g
                               (.drawString (str (nth args 0)) (+ x 48) (+ y 21))
                               (.drawString (str (nth args 1)) (+ x 73) (+ y 32))
                               (.drawString (str (nth args 2)) (+ x 83) (+ y 55))
                               (.drawString (str (nth args 3)) (+ x 73) (+ y 80))
                               (.drawString (str (nth args 4)) (+ x 48) (+ y 89))
                               (.drawString (str (nth args 5)) (+ x 23) (+ y 80))
                               (.drawString (str (nth args 6)) (+ x 13) (+ y 55))
                               (.drawString (str (nth args 7)) (+ x 23) (+ y 32)))
          :else nil)))

(defn draw-circle
  "Draws a circle circ at co-ordinates ((:x circ),(:y circ)) on the canvas."
  [circ]
  (let [g (.getGraphics (select main-window [:#canvas]))
        c circ
        x (:x circ)
        y (:y circ)
        op (cond (= (first (:circ circ)) +) "+"
                 (= (first (:circ circ)) -) "-"
                 (= (first (:circ circ)) *) (str \u00d7)
                 (= (first (:circ circ)) /) (str \u00f7)
                 (= (first (:circ circ)) \P) (str \P)
                 :else "error")
        args (rest (:circ circ))
        sym (:name circ)]
    (do
      (doto g
        (.setColor (java.awt.Color. 0x88FF88))
        (.fillOval x y 100 100)
        (.setColor java.awt.Color/BLACK)
        (.drawOval x y 100 100)
        (.drawLine (+ x 30) (+ y 5) (+ x 50) (+ y 50))
        (.setColor (java.awt.Color. 0x44BB44))
        (.fillOval (+ x 30) (+ y 30) 40 40)
        (.setColor java.awt.Color/BLACK)
        (.drawOval (+ x 30) (+ y 30) 40 40)
        (.drawLine (+ x 52) y (+ x 48) (- y 4)) ;; top arrow
        (.drawLine (+ x 52) y (+ x 48) (+ y 4)) ;; top arrow
        (.drawLine (+ x 100) (+ y 52) (+ x 96) (+ y 48)) ;; right arrow
        (.drawLine (+ x 100) (+ y 52) (+ x 104) (+ y 48)) ;; right arrow
        (.drawLine (+ x 48) (+ y 100) (+ x 52) (+ y 96)) ;; bottom arrow
        (.drawLine (+ x 48) (+ y 100) (+ x 52) (+ y 104)) ;; bottom arrow
        (.drawLine x (+ y 48) (- x 4) (+ y 52)) ;; left arrow
        (.drawLine x (+ y 48) (+ x 4) (+ y 52)) ;; left arrow
        (.drawString sym x (+ y 110))
        (.drawString op (+ x 46) (+ y 54)))
      (draw-args g args x y)
      (link-circles c))))

(defn detect-subs
  "Returns a list of maps containing start and end indices showing where a substring c appears in the superstring q."
  [c q]
  (loop [s (cond (= (subs q
                          (- (count q) (count c))
                          (count q)) c) (split q
                                               (re-pattern
                                                (escape
                                                 c
                                                 {\+ "\\+"
                                                  \( "\\("
                                                  \) "\\)"})))
                 :else (butlast (split q
                         (re-pattern
                          (escape
                           c
                           {\+ "\\+"
                            \( "\\("
                            \) "\\)"})))))
         i 0
         l '()]
    (cond (empty? s) (reverse l)
          (= c (subs q
                     (+ i (count (first s)))
                     (+ i (count (first s)) (count c)))) (recur (rest s)
                                                                (inc i)
                                                                (cons {:s (+ i (count (first s)))
                                                                       :e (+ i (count (first s)) (count c))}
                                                                      l))
          :else (recur s (inc i) l))))

;; (def tq (lisp-to-maths (eval (:q (first @current-qset)))))
;; (subs tq (:s (nth (detect-subs "1+2" tq) 0)) (:e (nth (detect-subs "1+2" tq) 0)))
;; (subs tq (:s (nth (detect-subs "1+2" tq) 1)) (:e (nth (detect-subs "1+2" tq) 1)))
;; (subs tq (:s (nth (detect-subs "1+2" tq) 2)) (:e (nth (detect-subs "1+2" tq) 2)))
;; (let [s "roflolmaomglolwtf" i (detect-subs "lol" s)] (subs s (:s (nth i 0)) (:e (nth i 0))))
;; (let [s "roflolmaomglolwtf" i (detect-subs "lol" s)] (subs s (:s (nth i 1)) (:e (nth i 1))))
;; (let [s "roflolmaomglolwtf" i (detect-subs "lol" s)] (subs s (:s (nth i 2)) (:e (nth i 2))))

;; (let [s "0123456789" i (detect-subs "456" s)] (subs s (:s (first i)) (:e (first i))))
;; (let [s "roflolmaomglolwtf" i (detect-subs "lol" s)] (subs s (:s (first i)) (:e (first i))))
;; (let [s "fagaha" i (detect-subs "a" s)] (subs s (:s (first i)) (:e (first i))))
;; (let [s "lalala" i (detect-subs "a" s)] (subs s (:s (first i)) (:e (first i))))
;; (detect-subs (lisp-to-maths (eval-circle (find-circle "c0"))) (lisp-to-maths (eval (:q (first @current-qset)))))
;; (let [s "dcbabcd" i (detect-subs "a" s)] (subs s (:s (first i)) (:e (first i))))
;; (let [s "dcbabcd" i (detect-subs "a" s)] (subs s (:s (nth i 1)) (:e (nth i 1))))

(defn highlight-text
  "Change the colour of the question text to c between characters s and e inclusive."
  [s e c]
  (loop [n s]
    (cond (>= n e) nil
          :else (do
                  (config! (select main-window [(keyword (str "#l" n))])
                           :foreground c)
                  (recur (inc n))))))

(defn unhighlight-text
  "Change the colour of all characters in :question to black."
  []
  (highlight-text 0 (count (lisp-to-maths (eval (:q (first @current-qset))))) "#000000"))

;; (highlight-text 4 10 "#0000FF")
;; (highlight-text 0 (count (lisp-to-maths (eval (:q (first @current-qset))))) "#000000")

(defn highlight
  "Highlights the circle circ, and the section of the question it represents."
  [circ]
  (cond (not (has-placeholders? circ))
  (let [x (:x circ)
        y (:y circ)]
    (do
      (doto (.getGraphics (select main-window [:#canvas]))
        (.setColor (java.awt.Color. 0x2ECCFA))
        (.fillOval (- x 3) (- y 3) 106 106))
      (loop [q (lisp-to-maths (eval (:q (first @current-qset))))
             i (detect-subs (lisp-to-maths (eval-circle circ)) q)]
        (cond (not (empty? i)) (do
                                 (highlight-text (:s (first i)) (:e (first i)) "#2ECCFA")
                                 (recur q (rest i)))))))))
              ;; :else (prn "i: empty"))))))

(comment
(defn entered-left-circle?
  "Checks whether the mouse has entered or left a circle by inspecting the contents of last-2-coords."
  []
  (let [p1 (not (nil? (point-in-circle (:x (nth @last-2-coords 0)) (:y (nth @last-2-coords 0)))))
        p2 (not (nil? (point-in-circle (:x (nth @last-2-coords 1)) (:y (nth @last-2-coords 1)))))]
    (cond (= p1 p2) false
          :else true)))
)

(defn clear-screen
  "Clears all visible drawings from the canvas."
  []
  (let [i (.getImage (java.awt.Toolkit/getDefaultToolkit) "images/bin.png")
        b (java.awt.image.BufferedImage. (.getWidth i) (.getHeight i) java.awt.image.BufferedImage/TYPE_INT_ARGB)
        t (java.awt.image.AffineTransformOp. (java.awt.geom.AffineTransform.) java.awt.image.AffineTransformOp/TYPE_NEAREST_NEIGHBOR)]
    (do
      (doto (.getGraphics b) (.drawImage i 0 0 nil))
      (doto (.getGraphics (select main-window [:#canvas]))
        (.setColor java.awt.Color/WHITE)
        ;; (.fillRect 15 15 (- screen-x 30) (- screen-y 30))))
        (.fillRect 0 0 screen-x screen-y)
        (.drawImage b t (- screen-x 100) (- screen-y 200))))))

(defn render
  "Clears the screen, then draws all circles currently in used-circles, linking nested circles together."
  []
  (do
    (clear-screen)
    (loop [u @used-circles]
      (cond (not (empty? u)) (do (draw-circle (first u))
                                 (recur (rest u)))))))

(def del-circle) ;; Declare del-circle, to be defined later

(defn add-arg
  "Returns the value of a circle c with the argument a added."
  [a c]
  {:x (:x c)
   :y (:y c)
   :name (:name c)
   :circ (concat (:circ c) (list a))})

(defn mod-xy
  "Returns the value of a circle c with the new co-ordinates x and y."
  [c x y]
  {:x x
   :y y
   :name (:name c)
   :circ (:circ c)})

(defn mod-arg
  "Returns the value of a circle c with the argument at (zero-indexed) index i replaced with a."
  [c i a]
  {:x (:x c)
   :y (:y c)
   :name (:name c)
   :circ (loop [in  (rest (:circ c))
                out (list (first (:circ c)))
                n   0]
           (cond (empty? in) (reverse out)
                 (= n i) (recur (rest in) (cons a out) (inc n))
                 :else (recur (rest in) (cons (first in) out) (inc n))))})

(defn mod-op
  "Returns the value of a circle c with the operator replaced with op."
  [c op]
  {:x (:x c)
   :y (:y c)
   :name (:name c)
   :circ (cons op (rest (:circ c)))})

(def check-answer) ;; Declare check-answer, to be defined later

(def update-answer) ;; Declare update-answer, to be defined later

(defn circ-drag-begin
  "Sends off the dragged circle to currently-dragging-circle."
  [e]
  (let [x  (.getX e)
        y  (.getY e)
        p  (point-in-circle x y)
        p? (not (nil? p))]
    (cond p? (do
               (del-circle p)
               (send-off used-circles (fn [a] (cons (mod-xy (find-circle p) (:x @current-coords) (:y @current-coords)) a)))))))

    ;; (cond c? (send-off currently-dragging-circle (fn [_] c)))))

(defn circ-drag-end
  "If dropped into another circle, removes the target circle and replaces it with a similar circle with the circle in currently-dragging-circle added as an argument.  Otherwise, moves the circle being dragged to the new position."
  [e]
  (let [x  (.getX e)
        y  (.getY e)
        c  (point-in-circle x y)
        c? (not (nil? c))]
    (cond c? (let [cp (add-arg (symbol @currently-dragging-circle) (find-circle c))]
               (do
                 (del-circle x y)
                 (await used-circles)
                 (send-off used-circles (fn [_] (cons cp @used-circles)))
                 (clear-screen)
                 (render)
                 (update-answer)))
          :else (let [cm (mod-xy (find-circle @currently-dragging-circle) (- x 50) (- y 50))]
                  (do
                    (del-circle (+ 10 (:x (find-circle @currently-dragging-circle))) (+ 10 (:y (find-circle @currently-dragging-circle))))
                    ;; (await used-circles)
                    (send-off used-circles (fn [_] (cons cm @used-circles)))
                    (clear-screen)
                    (render)
                    (update-answer))))))

(defn load-qset-init
  "Brings up a dialogue with a file chooser to specify where to load the question set from.  Sends off the contents of the chosen file to current-qset."
  []
  (choose-file :filters [(file-filter "Nico Question Set (*.nqs)"
                                      #(or (.isDirectory %)
                                           (=
                                            (apply str (take-last 4 (.toString (.getAbsoluteFile %))))
                                            ".nqs")))]
               :success-fn (fn [fc f]
                             (do
                               (send-off current-qset
                                         (fn [_]
                                           (read-qset
                                            (.getAbsoluteFile f))))
                               (send-off current-question
                                         (fn [_] 1))))))

(defn load-qset
  "Brings up a dialogue with a file chooser to specify where to load the question set from.  Sends off the contents of the chosen file to current-qset."
  [& a]
  (choose-file :filters [(file-filter "Nico Question Set (*.nqs)"
                                      #(or (.isDirectory %)
                                           (=
                                            (apply str (take-last 4 (.toString (.getAbsoluteFile %))))
                                            ".nqs")))]
               :success-fn (fn [fc f]
                             (do
                               (send-off current-qset
                                         (fn [_]
                                           (read-qset
                                            (.getAbsoluteFile f))))
                               (send-off current-question
                                         (fn [_] 1))
                               (config!
                                (select main-window [:#answer])
                                :text "0"
                                :foreground "#000000")
                               (config!
                                (select main-window [:#question])
                                :items [(string-to-panel (lisp-to-maths (eval (:q (get-q-no @current-question)))))]
                                :border (str "Question " @current-question))
                               (kill-used-circles)
                               (render)))))

(defn gen-circ-radios
  "Returns a horizontal-panel containing one radio button for each circle currently in used-circles.  Each radio button has the :id :(str pfx :name), and is in the button-group gp."
  [pfx gp]
  (horizontal-panel :id (keyword (str pfx "-circpanel"))
                    :items (loop [in  @used-circles
                                      out '()
                                  n   0]
                             (cond (empty? in) (into '[] out)
                                   :else (recur (rest in)
                                                (cons (radio :id (keyword (str pfx (:name (first in))))
                                                             :text (:name (first in))
                                                             :group gp
                                                             :selected? (cond (= n 0) true
                                                                              :else false))
                                                      out)
                                                (inc n))))))

;; Groups of radio buttons for use in new-dialogue
(def op (button-group))

(defn new-arg-panel
  "Creates the configuration panel for argument n to be used in new-dialogue."
  [n]
  (let [g  (button-group)
        gr (button-group)]
  (border-panel :id (keyword (str "args-selector-" n))
                :border (str "Argument " n)
                :north  (checkbox :id (keyword (str "arg" n "s?"))
                                  :text "Enabled?"
                                  :selected? (cond (< n 3) true
                                                   :else false))
                :center (horizontal-panel :items [(radio :id (keyword (str "arg" n "n?"))
                                                         :group g ;; (eval (symbol (str "arg" n)))
                                                         :selected? true)
                                                  (slider :id (keyword (str "arg" n "n"))
                                                          :orientation :horizontal
                                                          :value 0
                                                          :min -10
                                                          :max 10
                                                          :minor-tick-spacing 1
                                                          :major-tick-spacing 5
                                                          :snap-to-ticks? true
                                                          :paint-labels? true
                                                          :paint-track? true)])
                :south  (horizontal-panel :items [(radio :id (keyword (str "arg" n "c?"))
                                                         :group g) ;; (eval (symbol (str "arg" n))))
                                                  (gen-circ-radios (str "a" n) gr)])))) ;; (eval (symbol (str "arg" n "r"))))]))))

(def new-circle) ;; Declare new-circle, to be defined later

(defn new-dialogue
  "Generates the dialogue to be displayed as part of new-circle."
  []
  (do
    (native!)
    (border-panel :id :new-box
                  :north (horizontal-panel :id :name-op
                                           :items [;; (horizontal-panel :id :name-panel
                                                   ;;                   :border "Name"
                                                   ;;                   :items [(text :id :name-field)])
                                                   (horizontal-panel :id :op-select
                                                                     :border "Operator"
                                                                     :items [(radio :id :plus
                                                                                    :text "+"
                                                                                    :group op
                                                                                    :selected? true)
                                                                             (radio :id :minus
                                                                                    :text "-"
                                                                                    :group op)
                                                                             (radio :id :mul
                                                                                    :text (str \u00d7)
                                                                                    :group op)
                                                                             (radio :id :div
                                                                                    :text (str \u00f7)
                                                                                    :group op)])])
                  :center (vertical-panel :id :args-left
                                          :items [(new-arg-panel 1)
                                                  (new-arg-panel 3)
                                                  (new-arg-panel 5)
                                                  (new-arg-panel 7)])
                  :east  (vertical-panel :id :args-right
                                         :items [(new-arg-panel 2)
                                                 (new-arg-panel 4)
                                                 (new-arg-panel 6)
                                                 (new-arg-panel 8)]))))

(defn re-eval-box
  "Regenerate the bits of new-dialogue that need regenerating."
  []
  (do
    (config! (select new-dialogue [:#args-left]) :items [(new-arg-panel 1)
                                                         (new-arg-panel 3)
                                                         (new-arg-panel 5)
                                                         (new-arg-panel 7)])
    (config! (select new-dialogue [:#args-right]) :items [(new-arg-panel 2)
                                                          (new-arg-panel 4)
                                                          (new-arg-panel 6)
                                                          (new-arg-panel 8)])))

(defn new-circle
  "Brings up a dialogue box to configure a new circle.  Draws the circle on pressing 'OK'."
  [x y]
  (do
    (send-off used-circles (fn [a] (cons {:x (- x 50) :y (- y 50) :name (str "c" (count a)) :circ '(\P \P \P)} a)))
    (await used-circles)
    (clear-screen)
    (render)))

(comment
(defn new-circle
  "Brings up a dialogue box to configure a new circle.  Draws the circle on pressing 'OK'."
  [x y]
  (let [dlg (new-dialogue)]
    (invoke-later
     (-> (dialog :title "New",
                 :content dlg,
                 :on-close :dispose
                 :success-fn (fn [_] (let [circ {:x x
                                         :y y
                                         :name (str "c" (count @used-circles)) ;; (.getText (select dlg [:#name-field]))
                                         :circ (loop [op   (cond
                                                            (.isSelected (select dlg [:#plus])) +
                                                            (.isSelected (select dlg [:#minus])) -
                                                            (.isSelected (select dlg [:#mul])) *
                                                            (.isSelected (select dlg [:#div])) /
                                                            :else 'error)
                                                      s? (list
                                                          (.isSelected (select dlg [:#arg1s?]))
                                                          (.isSelected (select dlg [:#arg2s?]))
                                                          (.isSelected (select dlg [:#arg3s?]))
                                                          (.isSelected (select dlg [:#arg4s?]))
                                                          (.isSelected (select dlg [:#arg5s?]))
                                                          (.isSelected (select dlg [:#arg6s?]))
                                                          (.isSelected (select dlg [:#arg7s?]))
                                                          (.isSelected (select dlg [:#arg8s?])))
                                                      select-n (fn [n s] (select dlg [(keyword (str "#arg" n s))]))
                                                      select-c (fn [n c] (select dlg [(keyword (str "#a" n (:name c)))]))
                                                      get-circ (fn [n] (loop [in @used-circles]
                                                                         (cond (empty? in) nil
                                                                               (.isSelected (select-c n (first in))) (first in)
                                                                               :else (recur (rest in)))))
                                                      n 1
                                                      out (list op)]
                                                 (cond (empty? s?) (reverse out)
                                                       (first s?)  (recur op (rest s?) select-n select-c get-circ (inc n) (cons
                                                                                                                           (cond (.isSelected (select-n n "n?")) (.getValue (select-n n "n"))
                                                                                                                                 (.isSelected (select-n n "c?")) (symbol (:name (get-circ n)))
                                                                                                                                 :else "error")
                                                                                                                           out))
                                                       :else (recur op (rest s?) select-n select-c get-circ (inc n) out)))}]
                                       (do
                                         (send-off used-circles #(cons circ %))
                                         (dispose! dlg)
                                         (render)
                                         ;; (Thread/sleep 500) ;; Give them a chance to see their answer
                                         (update-answer))))
                 :cancel-fn (fn [_] (dispose! dlg)))
          pack!
          show!))))
)

(defn del-circle
  "Removes a circle from used-circles, such that it won't reappear on executing render."
  [x & y]
  (cond (string? x) (send-off used-circles (fn [a] (loop [in a
                                                         out '()]
                                                    (cond (empty? in) (reverse out)
                                                          (= x (:name (first in))) (recur (rest in) out)
                                                          :else (recur (rest in) (cons (first in) out))))))
        :else (send-off used-circles (fn [a] (loop [in  a
                                                   out '()
                                                   c   (point-in-circle x y)
                                                   c?  (not (nil? c))]
                                              (cond (not c?) nil ;; (alert "Circle not found.")
                                                    :else (cond (empty? in) (reverse out)
                                                                (= (:name (first in)) c) (recur (rest in) out c c?)
                                                                :else (recur (rest in) (cons (first in) out) c c?))))))))
    ;; (render)
    ;; (check-answer)))

(defn add-placeholder-arg
  "Adds a placeholder argument to an existing circle c."
  [c]
  (cond (< (count (rest (:circ c))) 8) (do
                                         (del-circle (:name c))
                                         (send-off used-circles (fn [a] (cons (add-arg \P c) a))))
        :else (alert "Each circle is allowed a maximum of 8 arguments.")))

(defn remove-last-arg
  "Removes the last argument from the circle c."
  [c]
  (cond (= (count (rest (:circ c))) 2) (alert "Each circle is allowed a minimum of 2 arguments.")
        :else (let [cp {:x (:x c)
                        :y (:y c)
                        :name (:name c)
                        :circ (butlast (:circ c))}]
                (do
                  (del-circle (:name c))
                  (send-off used-circles (fn [a] (cons cp a)))))))

(defn next-question
  "Loads the next question in the current question set."
  []
  (do
    (send-off current-question #(inc %))
    (await current-question)
    (get-q-no @current-question)
    (cond (not (nil? (get-q-no @current-question))) (do (config!
                                                         (select main-window [:#question])
                                                         :items [(string-to-panel (lisp-to-maths (first (rest (:q (get-q-no @current-question))))))]
                                                         :border (str "Question " @current-question))
                                                        (config!
                                                         (select main-window [:#answer])
                                                         :text "0"
                                                         :foreground "#000000"))
          :else (do
                  (alert "Well done!  Please choose another question set.")
                  (load-qset)))))

(defn question-right
  "To be executed when a question is answered correctly."
  []
  (do
    (config!
     (select main-window [:#answer])
     :foreground "#00BB00")
    (alert "Correct!")
    (next-question)
    (kill-used-circles)
    (await used-circles)
    (render)))

(defn question-wrong
  "To be executed when a question is answered incorrectly."
  []
  (config!
   (select main-window [:#answer])
   :foreground "#FF0000"))

(defn update-answer
  "Update the text in the :answer field to the current value of the diagram."
  []
  (config! (select main-window [:#answer])
           :text (cond (empty? @used-circles) (str 0)
                       :else (str (eval (eval-circle (find-root)))))))

(defn check-answer
  "Evaluate the current root circle and check against the answer to the current question, displaying the result to the user."
  []
  (do
    (update-answer)
    (cond (empty? @used-circles) (config! (select main-window [:#answer]) :text (str 0))
          (= (eval (eval-circle (find-root))) (eval (eval (:q (get-q-no @current-question))))) (question-right)
          :else (question-wrong))))

(defn new-circle-handler
  "Handler for new-circle-action that gets rid of the ActionEvent and makes new-circle actually usable in a context menu."
  [a]
  (let [xy @current-coords]
    (new-circle (:x xy) (:y xy))))

(defn del-circle-handler
  "Handler for del-circle-action that gets rid of the ActionEvent and makes del-circle actually usable in a context menu."
  [a]
  (let [xy @current-coords]
    (do
      (del-circle (:x xy) (:y xy))
      (render)
      (update-answer))))

(def new-circle-action
  ;; Action for adding a circle, for use in menus.
  (action :handler new-circle-handler
          :name "New circle"))

(def del-circle-action
  ;; Action for removing a circle, for use in menus.
  (action :handler del-circle-handler
          :name "Remove circle"))

(defn canvas-selected [e]
  "Handle a right mouse click on the canvas."
  (let [xy @current-coords
        x  (:x xy) ;; (.getX e)
        y  (:y xy) ;; (.getY e)
        c  (point-in-circle x y)
        c? (not (nil? c))]
    (cond c? [del-circle-action]
          :else [new-circle-action])))

(def main-window
  ;; Creates the contents of Nico's main window.
  (do
    (native!)
    (border-panel :id     :root
                  :west   (string-to-panel (lisp-to-maths (eval (:q (first @current-qset)))))
                  :center (label  :id     :answer
                                  :background "#FFFFFF"
                                  :foreground "#000000"
                                  :text   "0"
                                  :font   {:name :sans-serif :style :bold :size 18}
                                  :border "Answer")
                  :east   (button :id     :submit
                                  :background "#FFFFFF"
                                  :text   "Check"
                                  :listen [:mouse-clicked (fn [e] (check-answer))])
                  :south  (canvas       :id         :canvas
                                        :background "#FFFFFF"
                                        ;; :border     "Calculation"
                                        :size       [screen-x :by (- screen-y 100)]
                                        ;; :popup      #(canvas-selected %)
                                        :listen     [:mouse-moved    (fn [e] (let [x (.getX e)
                                                                                  y (.getY e)
                                                                                  p (point-in-circle x y)]
                                                                              (do
                                                                                (send-off current-coords (fn [_] {:x x :y y}))
                                                                                (clear-screen)
                                                                                (render)
                                                                                (cond (not (nil? p)) (do
                                                                                                       (highlight (find-circle p))
                                                                                                       (draw-circle (find-circle p)))
                                                                                      :else (unhighlight-text)))))
                                                     :mouse-clicked  (fn [e] (let [x     (.getX e) ;; x co-ord
                                                                                  y     (.getY e) ;; y co-ord
                                                                                  l?    (= (.getButton e) 1) ;; left-click?
                                                                                  r?    (= (.getButton e) 3) ;; right-click?
                                                                                  ctrl? (.isControlDown e) ;; ctrl-click?
                                                                                  c     (point-in-circle x y) ;; circle at (x,y)
                                                                                  c?    (not (nil? c)) ;; in a circle?
                                                                                  a     (arg-in-circle x y) ;; arg index at (x,y)
                                                                                  a?    (not (nil? a)) ;; on an arg?
                                                                                  p?    (cond a? (cond (= \P (nth (rest (:circ (find-circle c))) a)) true
                                                                                                       :else false)
                                                                                              :else false) ;; on a placeholder?
                                                                                  o?    (op-in-circle? x y)] ;; on the op?
                                                                              (cond c? (cond l? (cond ctrl? (remove-last-arg (find-circle c))
                                                                                                      :else (add-placeholder-arg (find-circle c)))
                                                                                             r? (cond a? (let [cc (find-circle c)
                                                                                                               in (eval (read-string (input "Number:")))]
                                                                                                           (do
                                                                                                             (del-circle c)
                                                                                                             (send-off used-circles (fn [ag] (cons (mod-arg cc a in) ag)))
                                                                                                             (clear-screen)
                                                                                                             (render)
                                                                                                             (check-answer)))
                                                                                                      o? (let [cc (find-circle c)
                                                                                                               in (eval (read-string (input "Operator:")))]
                                                                                                           (do
                                                                                                             (del-circle c)
                                                                                                             (send-off used-circles (fn [ag] (cons (mod-op cc in) ag)))
                                                                                                             (clear-screen)
                                                                                                             (render)
                                                                                                             (check-answer))))))))
                                                                                    ;; :else (cond l? (new-circle x y)))))
                                                     ;; :mouse-dragged  (fn [e] (let [x  (.getX e)
                                                     ;;                              y  (.getY e)
                                                     ;;                              c  (point-in-circle x y)
                                                     ;;                              l? (= (.getButton e) 1)
                                                     ;;                              r? (= (.getButton e) 3)]
                                                     ;;                          (cond l? (cond (not (nil? c)) (do
                                                     ;;                                                          (del-circle c)
                                                     ;;                                                          (send-off used-circles (fn [a] (cons (mod-xy (find-circle c) x y) a)))
                                                     ;;                                                          (clear-screen)
                                                     ;;                                                          (render)
                                                     ;;                                                          (link-circles (find-circle c))))
                                                     ;;                                :else (prn "right-drag!"))))
                                                     :mouse-pressed  (fn [e] (let [x (.getX e)
                                                                                  y (.getY e)
                                                                                  c (point-in-circle x y)
                                                                                  c? (not (nil? c))]
                                                                              (cond c? (send-off currently-dragging-circle (fn [_] (find-circle c)))
                                                                                    :else (new-circle x y))))]))))
                                                     ;; :mouse-released (fn [e] (let [x (.getX e)
                                                     ;;                              y (.getY e)
                                                     ;;                              c @currently-dragging-circle]
                                                     ;;                          (cond (and (>= x (- screen-x 100))
                                                     ;;                                     (>= y (- screen-y 200))) (del-circle c)
                                                     ;;                                 :else (let [fc (find-circle c)]
                                                     ;;                                         (do
                                                     ;;                                           (del-circle c)
                                                     ;;                                           (send-off used-circles (fn [a] (cons (mod-xy fc x y) a)))
                                                     ;;                                           (clear-screen)
                                                     ;;                                           (render)
                                                     ;;                                           (link-circles fc))))))]))))

                                                     (comment
                                                     :mouse-moved    (fn [e] (let [x (.getX e)
                                                                                  y (.getY e)
                                                                                  p (point-in-circle x y)]
                                                                              (do
                                                                                (send-off current-coords (fn [_] {:x x :y y}))
                                                                                (clear-screen)
                                                                                (render)
                                                                                (cond (not (nil? p)) (do
                                                                                                       (highlight (find-circle p))
                                                                                                       (draw-circle (find-circle p)))
                                                                                      :else (unhighlight-text)))))
                                                     :mouse-pressed (fn [e] (circ-drag-begin e))
                                                     :mouse-released (fn [e] (circ-drag-end e)))

                                                                     (comment
                                                                     (fn [e] (let [x  (.getX e)
                                                                                  y  (.getY e)
                                                                                  m  {:x x :y y}
                                                                                  p  (point-in-circle x y)
                                                                                  c  (cond (not (nil? p)) (find-circle p)
                                                                                        :else nil)
                                                                                  l? (nil? (point-in-circle
                                                                                            (:x (first @last-2-coords))
                                                                                            (:y (first @last-2-coords))))]
                                                                           (do
                                                                             (send-off last-2-coords (fn [_] (butlast (cons m @last-2-coords))))
                                                                             (await last-2-coords)
                                                                             (cond (and (entered-left-circle?) l?) (do
                                                                                                                           (clear-screen)
                                                                                                                           (render)
                                                                                                                           (highlight c)
                                                                                                                           (draw-circle c))
                                                                                   (and (entered-left-circle?) (not l?)) (do
                                                                                                                     (clear-screen)
                                                                                                                     (render)
                                                                                                                     (unhighlight-text))))))
                                                                     )

(def open-action
  ;; Action for opening a new question set, for use in menus.
  (action :handler load-qset
          :mnemonic \O
          :name "Open..."))

(def exit-action
  ;; Action for exiting Nico, for use in menus.
  (action :handler (fn [a] (System/exit 0))
          :mnemonic \E
          :name "Exit"))

(def about-action
  ;; Action for displaying the 'About' popup, for use in menus.
  (action :handler (fn [a] (alert (str "Nico v0.0.01\n
Copyright " \u00a9 " 2011-2012 Philip M. Yeeles\n
Nico is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the XFree Software Foundation, either version 3 of the License, or
(at your option) any later version.\n
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.\n
You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.")))
          :mnemonic \A
          :name "About..."))

(defn -main
  "Main method, initialises application."
  [& args]
  (do
    (System/setProperty "awt.useSystemAAFontSettings" "on")
    (System/setProperty "swing.aatext" "true")
    (native!)
    (load-qset-init)
    (invoke-later
      (do
        (-> (frame :title "Nico v0.0.1",
                   :menubar (menubar :items [(menu :text "File"
                                                   :mnemonic \F
                                                   :items [open-action exit-action])
                                             (menu :text "Help"
                                                   :mnemonic \H
                                                   :items [about-action])])
                   :content main-window,
                   :on-close :exit)
             pack!
             show!)
        (doto (.getGraphics (select main-window [:#canvas]))
          (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
          (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
          (.setRenderingHint RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY))
        (config! (select main-window [:#question])
                 :items [(string-to-panel (lisp-to-maths (eval (:q (first @current-qset)))))]
                 :border (str "Question " (:n (first @current-qset))))
        (clear-screen)
        (render)))))
