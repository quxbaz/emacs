;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-to-degrees

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/rewrite.el" user-emacs-directory))


;;; Helper

(defmacro to-degrees-test (input expected)
  "Push INPUT, run my/calc-to-degrees, compare result to EXPECTED."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,input))
     (my/calc-to-degrees)
     (should (equal (car (nth 1 calc-stack))
                    (math-read-expr ,expected)))))


;;; Pure pi multiples

(ert-deftest test-to-degrees-pi ()
  "pi -> 180."
  (to-degrees-test "pi" "180"))

(ert-deftest test-to-degrees-pi/2 ()
  "pi/2 -> 90."
  (to-degrees-test "pi / 2" "90"))

(ert-deftest test-to-degrees-pi/4 ()
  "pi/4 -> 45."
  (to-degrees-test "pi / 4" "45"))

(ert-deftest test-to-degrees-pi/3 ()
  "pi/3 -> 60."
  (to-degrees-test "pi / 3" "60"))

(ert-deftest test-to-degrees-pi/6 ()
  "pi/6 -> 30."
  (to-degrees-test "pi / 6" "30"))

(ert-deftest test-to-degrees-2pi ()
  "2*pi -> 360."
  (to-degrees-test "2 * pi" "360"))


;;; Symbolic expressions

(ert-deftest test-to-degrees-n-pi ()
  "n*pi -> 180*n."
  (to-degrees-test "n * pi" "180 * n"))

(ert-deftest test-to-degrees-frac-n-pi ()
  "2/3*n*pi -> 120*n."
  (to-degrees-test "2:3 * n * pi" "120 * n"))
