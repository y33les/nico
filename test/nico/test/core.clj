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

(ns nico.test.core
  (:use [nico.core])
  (:use [clojure.test]))

;; (deftest replace-me ;; FIXME: write
;;   (is false "No tests have been written."))
;; ×
;; ÷

(prn (System/getProperty "user.dir"))

(def t0 {:x 100 :y 100 :name "t0" :circ '(+ 1 2)})
(def t1 {:x 250 :y 250 :name "t1" :circ '(+ 4 t0 7 8)})
(def t2 {:x 400 :y 400 :name "t2" :circ '(+ \? 4)})
(def t3 {:x 550 :y 550 :name "t3" :circ '(+ \? \?)})

(send-off used-circles (fn [_] '({:x 100 :y 100 :name "t0" :circ '(+ 1 2)} {:x 250 :y 250 :name "t1" :circ '(+ 4 t0 7 8)})))

(deftest lisp-maths
  (is (= (lisp-to-maths '(+ 2 3)) "2+3"))
  (is (= (lisp-to-maths '(- 2 3)) "2-3"))
  (is (= (lisp-to-maths '(* 2 3)) "2×3"))
  (is (= (lisp-to-maths '(/ 2 3)) "2÷3"))
  (is (= (lisp-to-maths '(+ 19 28 30 1 -4)) "19+28+30+1+-4"))
  (is (= (lisp-to-maths '(+ (- 2 3) (* 4 5))) "(2-3)+(4×5)"))
  (is (= (lisp-to-maths '(+ 3 4 (- 2 1) 1)) "3+4+(2-1)+1")))

(deftest qs-read
  (is (= (read-qset "test/nico/test/test.nqs") '({:n 1 :q '(+ 1 2 3 4 5) :a? false :c? false} {:n 2 :q '(- 2 (+ 4 5) 3) :a? false :c? false}))))

(deftest has-ph
  (is (= false (has-placeholders? t0)))
  (is (= true (has-placeholders? t2)))
  (is (= true (has-placeholders? t3))))

(deftest rm-ph
  (is (= '(+ 1 2) (:circ (remove-placeholders t0))))
  (is (= '(+ 4) (:circ (remove-placeholders t2))))
  (is (= '(+) (:circ (remove-placeholders t3)))))

(deftest nest-test
  (is (= '() (nested-circles t0)))
  (is (= '("t0") (nested-circles t1))))

(deftest pt-in-circ
  (is (= nil (point-in-circle 9999 9999)))
  (is (= "t0" (point-in-circle 175 175))))

(deftest subs-det
  (is (= '({:s 1 :e 2} {:s 3 :e 4} {:s 5 :e 6}) (detect-subs "a" "xayaza")))
  (is (= '({:s 3 :e 6}) (detect-subs "bar" "foobarbazquux"))))

(deftest addarg
  (is (= {:x 100 :y 100 :name "t0" :circ '(+ 1 2 3)} (add-arg 3 {:x 100 :y 100 :name "t0" :circ '(+ 1 2)}))))

(deftest modarg
  (is (= {:x 100 :y 100 :name "t0" :circ '(+ 7 2)} (mod-arg {:x 100 :y 100 :name "t0" :circ '(+ 1 2)} 0 7))))

(deftest modxy
  (is (= {:x 800 :y 900 :name "t0" :circ '(+ 1 2)} (mod-xy {:x 100 :y 100 :name "t0" :circ '(+ 1 2)} 800 900))))

(deftest modop
  (is (= {:x 100 :y 100 :name "t0" :circ '(- 1 2)} (mod-op {:x 100 :y 100 :name "t0" :circ '(+ 1 2)} '-))))
