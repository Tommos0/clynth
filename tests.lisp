(ql:quickload "fiveam")

(defpackage :clynth/tests
    (:use :cl :fiveam :clynth))

(in-package :clynth/tests)

(def-suite linear-slide
  :description "Bladiebla")

(in-suite linear-slide)

(test can-call-once
  (is (= 1 1)))

(run-all-tests)
