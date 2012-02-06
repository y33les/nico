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
  (:use (seesaw core graphics chooser)
        [clojure.string :only [split split-lines]]))

(def screen-x
  ;; Available screen width.
  (int (.getWidth (.getScreenSize (java.awt.Toolkit/getDefaultToolkit)))))

(def screen-y
  ;; Available screen height.
  (int (.getHeight (.getScreenSize (java.awt.Toolkit/getDefaultToolkit)))))

(def used-circles
  ;; Agent listing symbols pointing to existing circles.
  (agent '()))

(def current-qset
  ;; Agent listing question set read from a file.
  (agent '()))

(def current-question
  ;; Agent containing an integer corresponding to the quesiton number.
  (agent 0))

(defn kill-used-circles []
  "Empties used-circles.  For use in debugging; should be removed from finished program."
  (def used-circles (agent '())))

(defn kill-current-qset []
  "Empties current-qset.  For use in debugging; should be removed from finished program."
  (def current-qset (agent '())))

(defn lisp-to-maths [sexp]
  "Translates a sexp into a string of child-readable maths."
  (loop [s   (rest sexp)
         op  (cond (= (eval (first sexp)) +) "+"
                   (= (eval (first sexp)) -) "-"
                   (= (eval (first sexp)) *) (str \u00d7)
                   (= (eval (first sexp)) /) (str \u00f7)
                   :else "error")
         out ""
         n 0]
    (cond (empty? s) (str out "")
          (odd? n) (recur s op (str out op) (inc n))
          (list? (first s)) (recur (rest s) op (str out "(" (lisp-to-maths (first s)) ")") (inc n))
          :else (recur (rest s) op (str out (first s)) (inc n)))))

(defn find-circle [name]
  "Returns the circle in used-circles with the :name property corresponding to name."
  (loop [n name
         u @used-circles]
    (cond (empty? u) (alert "Circle not found.")
          (= n (:name (first u))) (first u)
          :else (recur n (rest u)))))

(defn read-qset [qsfile]
  "Reads a set of questions from the file qsfile into a list of maps."
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

(defn get-q-no [num]
  "Gets the question from the current set with the :n field of value num."
  (loop [qs @current-qset
         n  num]
    (cond (empty? qs) nil
          (= n (:n (first qs))) (first qs)
          :else (recur (rest qs) n))))    

(defn nested? [circ]
  "Returns true if a circle contains other circles."
  (cond (not (map? circ)) false
        :else (loop [c (:circ circ)]
                (cond (empty? c) false
                      (symbol? (first c)) true
                      :else (recur (rest c))))))

(defn eval-circle [circ]
  "Iterates across a circle list, resolving symbols into their respective circles."
  (loop [c (:circ circ)
         out '()]
    (cond (empty? c) (reverse out)
          (symbol? (first c)) (cond (nested? (find-circle (str (first c)))) (recur (rest c) (cons (eval-circle (find-circle (str (first c)))) out))
                                    :else (recur (rest c) (cons (:circ (find-circle (str (first c)))) out)))
          :else (recur (rest c) (cons (first c) out)))))

(def main-window)

(defn in-circle [x x?]
  "Checks if the supplied co-ordinate is within a circle (checks against :x if x? is true, :y otherwise) and, if so, returns the name of that circle as a string (otherwise returns nil)."
  (loop [u @used-circles]
    (cond x? (cond (empty? u) nil
                   (and (> x (:x (first u)))
                        (< x (+ (:x (first u)) 100))) (:name (first u))
                   :else (recur (rest u)))
          :else (cond (empty? u) nil
                      (and (> x (:y (first u)))
                           (< x (+ (:y (first u)) 100))) (:name (first u))
                      :else (recur (rest u))))))

(defn in-circle? [x x?]
  "Wrapper function for in-circle that returns true if x is within the horizontal (if x? is true, vertical if x? is false) range of any circle and false otherwise."
  (not (nil? (in-circle x x?))))

(defn point-in-circle [x y]
  "Checks if the supplied co-ordinates are within a circle and, if so, returns the name of that circle (else nil)."
  (loop [u @used-circles]
    (cond (empty? u) nil
          (and (> x (:x (first u)))
               (< x (+ (:x (first u)) 100))
               (> y (:y (first u)))
               (< y (+ (:y (first u)) 100))) (:name (first u))
          :else (recur (rest u)))))

(defn available? [x x?]
  "Wrapper function for in-circle? that also checks if the generated co-ordinate will be out of bounds.  If x? is true it checks a x co-ordinate, if not it checks a y co-ordinate."
  (cond x? (not (or (in-circle? x x?) (> (+ x 100) screen-x) (in-circle (+ x 100) x?)))
        :else (not (or (in-circle? x x?) (> (+ x 100) screen-y) (in-circle (+ x 100) x?)))))

(comment
(defn count-nested [circ]
  "Returns an integer corresponding to how many nests of circles a given circle contains."
  (loop [c (rest (:circ circ))
         n 1]
    (cond (empty? c) n
          ;; (symbol? (first c)) (cond (nested? (find-circle (str (first c)))) (recur (rest c) (+ n (count-nested (find-circle (str (first c))))))
          ;;                           :else (recur (rest c) (inc n)))
          (symbol? (first c)) (recur (rest c) (+ n (count-nested (find-circle (str (first c))))))
          :else (recur (rest c) n))))
)

(defn leaf? [circ]
  "Returns true if the circle does not have any other circles as arguments."
  (loop [c (rest (:circ circ))
         l true]
    (cond (empty? c) l
          (symbol? (first c)) false
          :else (recur (rest c) l))))

(defn nested-circles [circ]
  "Returns a list of circ's arguments that are other circles."
  (loop [in  (rest (:circ circ))
         out '()]
    (cond (empty? in) (reverse out)
          (symbol? (first in)) (recur (rest in) (cons (str (first in)) out))
          :else (recur (rest in) out))))

(defn is-arg? [circ1 circ2]
  "Returns true if circ1 is an argument of circ2."
  (loop [n (:name circ1)
         c (rest (:circ circ2))
         a false]
    (cond (empty? c) a
          (= n (str (first c))) true
          :else (recur n (rest c) a))))

(defn root? [circ]
  "Returns true if circ is not used as an argument to any other circle."
  (loop [c circ
         u @used-circles
         r true]
    (cond (empty? u) r
          (is-arg? c (first u)) false
          :else (recur c (rest u) r))))

(defn find-root []
  "Returns the root circle (i.e. the circle to be evaluated that contains all others)."
  (loop [u @used-circles
         c nil]
    (cond (empty? u) c
          (root? (first u)) (first u)
          :else (recur (rest u) c))))

(defn find-longest-path [c]
  "Find the longest path to the circle c."
  '())

(defn count-nested [circ]
  "Returns an integer representing the number of levels traversed before hitting a root node."
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

(defn link-circles [circ]
  "Draws a lines from a nested circle to its circle-valued arguments."
  (loop [c (rest (:circ circ))
         x (+ (:x circ) 50)
         y (+ (:y circ) 50)]
    (cond (empty? c) nil
          (symbol? (first c)) (do (let [t  (find-circle (str (first c)))
                                        tx (+ (:x t) 50)
                                        ty (+ (:y t) 50)]
                                  (doto (.getGraphics (select main-window [:#canvas]))
                                    (.drawLine x y tx ty)))
                                  (recur (rest c) x y))
          :else (recur (rest c) x y))))

(defn draw-args [g args x y]
  "Draws the arguments of an expression around its circle."
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
        :else nil))

(defn draw-circle [circ]
  "Draws a circle circ at co-ordinates ((:x circ),(:y circ)) on the canvas."
  (let [g (.getGraphics (select main-window [:#canvas]))
        c circ
        x (:x circ)
        y (:y circ)
        op (cond (= (first (:circ circ)) +) "+"
                 (= (first (:circ circ)) -) "-"
                 (= (first (:circ circ)) *) (str \u00d7)
                 (= (first (:circ circ)) /) (str \u00f7)
                 :else "error")
        args (rest (:circ circ))
        sym (:name circ)]
    (do
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
        (.drawString op (+ x 46) (+ y 54)))
      (draw-args g args x y)
      (link-circles c))))

(defn clear-screen []
  "Clears all visible drawings from the canvas."
  (doto (.getGraphics (select main-window [:#canvas]))
    (.setColor java.awt.Color/WHITE)
    (.fillRect 15 15 (- screen-x 30) (- screen-y 30))))

(defn render []
  "Clears the screen, then draws all circles currently in used-circles, linking nested circles together."
  (do
    (clear-screen)
    (loop [u @used-circles]
      (cond (not (empty? u)) (do (draw-circle (first u))
                                 (recur (rest u)))))))

(defn load-qset-init []
  "Brings up a dialogue with a file chooser to specify where to load the question set from.  Sends off the contents of the chosen file to current-qset."
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
                         
(defn load-qset [& a]
  "Brings up a dialogue with a file chooser to specify where to load the question set from.  Sends off the contents of the chosen file to current-qset."
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
                                :text (lisp-to-maths (eval (:q (get-q-no @current-question))))
                                :border (str "Question " @current-question))
                               (kill-used-circles)
                               (render)))))

(defn gen-circ-radios [pfx gp]
  "Returns a horizontal-panel containing one radio button for each circle currently in used-circles.  Each radio button has the :id :(str pfx :name), and is in the button-group gp."
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

(defn new-arg-panel [n]
  "Creates the configuration panel for argument n to be used in new-dialogue."
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

(defn new-dialogue []
  "Generates the dialogue to be displayed as part of new-circle."
  (do
    (native!)
    (border-panel :id :new-box
                  :north (horizontal-panel :id :name-op
                                           :items [(horizontal-panel :id :name-panel
                                                                     :border "Name"
                                                                     :items [(text :id :name-field)])
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

(defn re-eval-box []
  "Regenerate the bits of new-dialogue that need regenerating."
  (do
    (config! (select new-dialogue [:#args-left]) :items [(new-arg-panel 1)
                                                         (new-arg-panel 3)
                                                         (new-arg-panel 5)
                                                         (new-arg-panel 7)])
    (config! (select new-dialogue [:#args-right]) :items [(new-arg-panel 2)
                                                          (new-arg-panel 4)
                                                          (new-arg-panel 6)
                                                          (new-arg-panel 8)])))

(def check-answer) ;; Declare check-answer, to be defined later

(defn new-circle [x y]
  "Brings up a dialogue box to configure a new circle.  Draws the circle on pressing 'OK'."
  (let [dlg (new-dialogue)]
    (invoke-later
     (-> (dialog :title "New",
                 :content dlg,
                 :on-close :dispose
                 :success-fn (fn [_] (let [circ {:x x
                                         :y y
                                         :name (.getText (select dlg [:#name-field]))
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
                                         (render)
                                         (check-answer))))
                 :cancel-fn (fn [_] (dispose! dlg)))
          pack!
          show!))))

(defn del-circle [x y & a]
  "Removes a circle from used-circles, such that it won't reappear on executing render."
  (do
    (send-off used-circles (fn [_] (loop [in @used-circles
                                          out '()
                                          c (cond (string? (first a)) (first a)
                                                  (instance? java.awt.event.ActionEvent (first a)) (point-in-circle x y);; (:x @current-click) (:y @current-click))
                                                  :else (input "Remove:"))]
                                     (cond (empty? in) (reverse out)
                                           (= (:name (first in)) c) (recur (rest in) out c)
                                           :else (recur (rest in) (cons (first in) out) c)))))
    (render)
    (check-answer)))

(defn next-question []
  "Loads the next question in the current question set."
  (do
    (send-off current-question #(inc %))
    (await current-question)
    (get-q-no @current-question)
    (cond (not (nil? (get-q-no @current-question))) (do (config!
                                                         (select main-window [:#question])
                                                         :text (lisp-to-maths (first (rest (:q (get-q-no @current-question)))))
                                                         :border (str "Question " @current-question))
                                                        (config!
                                                         (select main-window [:#answer])
                                                         :text "0"
                                                         :foreground "#000000"))
          :else (do
                  (alert "Well done!  Please choose another question set.")
                  (load-qset)))))

(defn question-right []
  "To be executed when a question is answered correctly."
  (do
    (config!
     (select main-window [:#answer])
     :text (str (eval (eval-circle (find-root))))
     :foreground "#00FF00")
    (alert "Correct!")
    (next-question)
    (kill-used-circles)
    (await used-circles)
    (render)))

(defn question-wrong []
  "To be executed when a question is answered incorrectly."
  (config!
   (select main-window [:#answer])
   :text (str (eval (eval-circle (find-root))))
   :foreground "#FF0000"))

(defn check-answer []
  "Evaluate the current root circle and check against the answer to the current question, displaying the result to the user."
  (cond (empty? @used-circles) (config! (select main-window [:#answer]) :text (str 0))
        (= (eval (eval-circle (find-root))) (eval (eval (:q (get-q-no @current-question))))) (question-right)
        :else (question-wrong)))

(def main-window
  ;; Creates the contents of Nico's main window.
  (do
    (native!)
    (border-panel :id     :root
                  :center (label  :id     :question
                                  :text   (str
                                           "Q"
                                           (:n (first @current-qset))
                                           ": "
                                           (:q (first @current-qset)))
                                  :font   {:name :sans-serif :style :bold :size 18}
                                  :border "Question")
                  :east   (label  :id     :answer
                                  :text   "0"
                                  :font   {:name :sans-serif :style :bold :size 18}
                                  :border "Answer")
                  :south  (canvas       :id         :canvas
                                        :background "#FFFFFF"
                                        :border     "Calculation"
                                        :size       [screen-x :by (- screen-y 100)]
                                        :listen     [:mouse-clicked (fn [e] (let [x (.getX e)
                                                                                  y (.getY e)]
                                                                              (cond (nil? (point-in-circle x y)) (do
                                                                                                                   (new-circle x y)
                                                                                                                   (render)
                                                                                                                   (check-answer))
                                                                                    :else (do
                                                                                            (del-circle x y) ;; (point-in-circle x y))
                                                                                            (render)
                                                                                            (check-answer)))))]))))

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
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.\n
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.\n
You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.")))
          :mnemonic \A
          :name "About..."))

(defn -main [& args]
  (do
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
       (config! (select main-window [:#question]) :text (lisp-to-maths (first (rest (:q (first @current-qset))))) :border (str "Question " (:n (first @current-qset))))))))