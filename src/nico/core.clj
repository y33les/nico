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

(def used-circles
  ;; Agent listing symbols pointing to existing circles.
  (agent '()))

(def current-qset
  ;; Agent listing question set read from a file.
  (agent '()))

(def current-question
  ;; Agent containing an integer corresponding to the quesiton number.
  (agent 0))

(def currently-dragging-circle
  ;; Agent containing a string corresponding to the name of the circle currently being dragged.
  (agent nil))

(def current-click
  ;; Agent containing a map of the co-ordinates on the canvas where the mouse was clicked.
  (agent {:x 0 :y 0}))

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

(defn string-to-int-list [s]
  "Takes a string of integers separated by spaces as returns a list of integers."
  (loop [in (split s #" ")
         out '()]
    (cond (empty? in) (reverse out)
          :else (recur (rest in) (cons (Integer/parseInt (first in)) out)))))

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
  (cond x? (not (or (in-circle? x x?) (> (+ x 100) 640) (in-circle (+ x 100) x?)))
        :else (not (or (in-circle? x x?) (> (+ x 100) 480) (in-circle (+ x 100) x?)))))

(defn xy-rng []
  "Generates a pair of co-ordinates as a map that can be used to create a non-overlapping circle."
  (let [r (java.util.Random.)]
    {:x (loop [rx (+ 15 (. r nextInt 510))]
          (cond (available? rx true) rx
                :else (recur (+ 15 (. r nextInt 510)))))
     :y (loop [ry (+ 15 (. r nextInt 340))]
          (cond (available? ry false) ry
                :else (recur (+ 15 (. r nextInt 340)))))}))

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

(defn find-longest-path [circ]
  "Takes a nested circle and returns the longest path to a root circle."
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
        (.drawString op (+ x 46) (+ y 54))
        (.drawString (str args) x y))
      (link-circles c))))

(defn clear-screen []
  "Clears all visible drawings from the canvas."
  (doto (.getGraphics (select main-window [:#canvas]))
    (.setColor java.awt.Color/WHITE)
    (.fillRect 15 15 610 450)))

(defn render []
  "Clears the screen, then draws all circles currently in used-circles, linking nested circles together."
  (do
    (clear-screen)
    (loop [u @used-circles]
      (cond (not (empty? u)) (do (draw-circle (first u))
                                 ;; (link-circles (first u))
                                 (recur (rest u)))))
    (loop [u @used-circles
           y 20]
      (cond (not (empty? u)) (do (doto (.getGraphics (select main-window [:#canvas]))
                                   (.drawString (str (:name (last u)) ": " (eval (eval-circle (last u)))) 20 y))
                                 (recur (butlast u) (+ y 10)))))))

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
                         
(defn load-qset []
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

(def new-dialogue
  ;; The dialogue to be displayed as part of new-circle.
  (let [op   (button-group)
        arg1 (button-group)
        arg2 (button-group)
        arg3 (button-group)
        arg4 (button-group)
        arg5 (button-group)
        arg6 (button-group)
        arg7 (button-group)
        arg8 (button-group)]
    (do
      (native!)
      (border-panel :id :new-box
                    :north (horizontal-panel :id :op-select
                                             :border "Operator"
                                             :items [(radio :text "+" :group op :selected? true)
                                                     (radio :text "-" :group op)
                                                     (radio :text (str \u00d7) :group op)
                                                     (radio :text (str \u00f7) :group op)])
                    :center (grid-panel :id :arg-type-select
                                        :columns 2
                                        :items [(label "Argument 1")
                                                (horizontal-panel :items [(radio :text "Number" :group arg1 :selected? true)
                                                                          (radio :text "Circle" :group arg1)])
                                                (slider :id :arg1n
                                                        :orientation :horizontal
                                                        :value 0
                                                        :min -10
                                                        :max 10
                                                        :minor-tick-spacing 1
                                                        :major-tick-spacing 5
                                                        :snap-to-ticks? true
                                                        :paint-labels? true
                                                        :paint-track? true)
                                                (label "Argument 2")
                                                (horizontal-panel :items [(radio :text "Number" :group arg2 :selected? true)
                                                                          (radio :text "Circle" :group arg2)])
                                                (label "Argument 3")
                                                (horizontal-panel :items [(radio :text "Number" :group arg3)
                                                                          (radio :text "Circle" :group arg3)
                                                                          (radio :text "None" :group arg3 :selected? true)])
                                                (label "Argument 4")
                                                (horizontal-panel :items [(radio :text "Number" :group arg4)
                                                                          (radio :text "Circle" :group arg4)
                                                                          (radio :text "None" :group arg4 :selected? true)])
                                                (label "Argument 5")
                                                (horizontal-panel :items [(radio :text "Number" :group arg5)
                                                                          (radio :text "Circle" :group arg5)
                                                                          (radio :text "None" :group arg5 :selected? true)])
                                                (label "Argument 6")
                                                (horizontal-panel :items [(radio :text "Number" :group arg6)
                                                                          (radio :text "Circle" :group arg6)
                                                                          (radio :text "None" :group arg6 :selected? true)])
                                                (label "Argument 7")
                                                (horizontal-panel :items [(radio :text "Number" :group arg7)
                                                                          (radio :text "Circle" :group arg7)
                                                                          (radio :text "None" :group arg7 :selected? true)])
                                                (label "Argument 8")
                                                (horizontal-panel :items [(radio :text "Number" :group arg8)
                                                                          (radio :text "Circle" :group arg8)
                                                                          (radio :text "None" :group arg8 :selected? true)])])))))

(defn test-new-box [& args]
  (do
    (native!)
    (invoke-later
     (do
       (-> (frame :title "New",
                  :content new-dialogue,
                  :on-close :dispose)
           pack!
           show!)))))

(defn new-circle [& e]
  "Brings up a dialogue to define and draw a new circle on the Calculation canvas."
  (let [in   (input "New:")
        rng  (xy-rng)
        name (first (split in #" "))
        fun  (eval (read-string (nth (split in #" ") 1)))
        expr (cons fun (rest (rest (read-string (str "(" in ")")))))
        circ {:x (:x @current-click);;(cond (not (nil? e)) (:x rng)
                 ;;      :else (.getX e))
              :y (:y @current-click);;(cond (not (nil? e)) (:y rng)
                 ;;      :else (.getY e))
              :name name
              :circ expr}]
       (do
         (send-off used-circles #(cons circ %))
         (await used-circles)
         (draw-circle (find-circle name)))))

(defn del-circle [& a]
  "Removes a circle from used-circles, such that it won't reappear on executing render."
  ;; (do
    (send-off used-circles (fn [_] (loop [in @used-circles
                                          out '()
                                          c (cond (string? (first a)) (first a)
                                                  (instance? java.awt.event.ActionEvent (first a)) (point-in-circle (:x @current-click) (:y @current-click))
                                                  :else (input "Remove:"))]
                                     (cond (empty? in) (reverse out)
                                           (= (:name (first in)) c) (recur (rest in) out c)
                                           :else (recur (rest in) (cons (first in) out) c))))))
    ;; (await-for 2000 used-circles)
    ;; (render)))

(defn edit-circle [& e]
  "Brings up a dialogue to edit the parameters of an existing circle and redraws it."
  (let [in   (input "Edit:")
        name (cond (not (nil? e)) (point-in-circle (:x @current-click) (:y @current-click))
                   :else (first (split in #" ")))
        fun  (eval (read-string (nth (split in #" ") 1)))
        expr (cons fun (rest (rest (read-string (str "(" in ")")))))
        old  (find-circle name)
        new  {:x (:x old)
              :y (:y old)
              :name (:name old)
              :circ expr}]
    (do
      (prn name)
      (del-circle name)
      (send-off used-circles #(cons new %))
      (link-circles (find-circle name))
      (render))))

(defn drag-circle-begin [e]
  "Detects if a circle has been selected and, if so, sends off its name to currently-dragging-circle."
  (let [x  (.getX e)
        y  (.getY e)
        c  (point-in-circle x y)
        c? (not (nil? c))]
    (cond c? (send-off currently-dragging-circle (fn [_] c)))))

(defn drag-circle-end [e]
  "When a circle being dragged is released, modifies its :x and :y fields and sends off nil to currently-dragging-circle."
  (let [x   (.getX e)
        y   (.getY e)
        old (find-circle @currently-dragging-circle)
        new {:x x :y y :name (:name old) :circ (:circ old)}]
    (cond (not (nil? @currently-dragging-circle)) (do
                                                    (del-circle (:name old))
                                                    (send-off used-circles #(cons new %))
                                                    (send-off currently-dragging-circle (fn [_] nil))))))

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
  (cond (= (eval (eval-circle (find-root))) (eval (eval (:q (get-q-no @current-question))))) (question-right)
        :else (question-wrong)))

(def new-circle-action
  ;; Action for adding a circle, for use in menus.
  (action :handler new-circle
          :name "New circle"))

(def edit-circle-action
  ;; Action for editing a circle, for use in menus.
  (action :handler edit-circle
          :name "Edit circle"))

(def del-circle-action
  ;; Action for removing a circle, for use in menus.
  (action :handler del-circle
          :name "Remove circle"))

(defn canvas-selected [e]
  "Handle a right mouse click on the canvas."
  (let [x  (.getX e)
        y  (.getY e)
        c  (point-in-circle x y)
        c? (not (nil? c))]
    (cond c? [edit-circle-action del-circle-action]
          :else [new-circle-action])))
    
(def main-window
  ;; Creates the contents of Nico's main window.
  (do
    (native!)
    (border-panel :id     :root
                  :north  (border-panel :id       :top
                                        :center   (label  :id     :question
                                                          :text   (str
                                                                   "Q"
                                                                   (:n (first @current-qset))
                                                                   ": "
                                                                   (:q (first @current-qset)))
                                                          :font   {:name :sans-serif :style :bold :size 18}
                                                          :border "Question")
                                        :east     (label  :id     :answer
                                                          :text   "0"
                                                          :font   {:name :sans-serif :style :bold :size 18}
                                                          :border "Answer"))
                  :center (canvas       :id         :canvas
                                        :background "#FFFFFF"
                                        :border     "Calculation"
                                        :size       [640 :by 480]
                                        :popup      #(canvas-selected %)
                                        :listen     [:mouse-moved (fn [e] (send-off current-click (fn [_] {:x (- (.getX e) 50) :y (- (.getY e) 50)})))])
                                        ;; :listen     [:mouse-pressed #(drag-circle-begin %)
                                        ;;              :mouse-released #(do
                                        ;;                                 (drag-circle-end %)
                                        ;;                                 (render))])
                  :east   (grid-panel   :id         :buttons
                                        :columns    1
                                        :items      [(button :id     :open
                                                             :text   "Open"
                                                             :listen [:mouse-clicked (fn [e]
                                                                                       (do (load-qset)))])
                                                     (button :id     :new
                                                             :text   "New"
                                                             :listen [:mouse-clicked (fn [e]
                                                                                       (do
                                                                                         (new-circle)
                                                                                         (render)
                                                                                         (check-answer)))])
                                                     (button :id     :edit
                                                             :text   "Edit"
                                                             :listen [:mouse-clicked (fn [e]
                                                                                       (do
                                                                                         (edit-circle)
                                                                                         (render)
                                                                                         (check-answer)))])
                                                     (button :id     :remove
                                                             :text   "Remove"
                                                             :listen [:mouse-clicked (fn [e]
                                                                                       (do
                                                                                         (del-circle)
                                                                                         (render)
                                                                                         (check-answer)))])
                                                     (button :id     :render
                                                             :text   "Render"
                                                             :listen [:mouse-clicked (fn [e]
                                                                                       (render))])
                                                     (button :id     :clear
                                                             :text   "Clear"
                                                             :listen [:mouse-clicked (fn [e]
                                                                                       (clear-screen))])])
                  :south  (button       :id         :eval
                                        :text       "Evaluate!"
                                        :font       {:name :sans-serif :style :bold :size 16}
                                        :listen [:mouse-clicked (fn [e] (check-answer))]))))

(defn -main [& args]
  (do
    (native!)
    (load-qset-init)
    (invoke-later
     (do
       (-> (frame :title "Nico v0.0.1",
                  :content main-window,
                  :on-close :exit)
           pack!
           show!)
       (config! (select main-window [:#question]) :text (lisp-to-maths (first (rest (:q (first @current-qset))))) :border (str "Question " (:n (first @current-qset))))))))