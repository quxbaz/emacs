;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-poly-roots
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))


;;; Helper

(defmacro my-calc-poly-roots-test (input expected)
  "Push INPUT, run my/calc-poly-roots, compare top-of-stack to EXPECTED."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,input))
     (my/calc-poly-roots)
     (should (equal (car (nth 1 calc-stack))
                    (math-read-expr ,expected)))))


;;; Expression forms

(ert-deftest test-my/calc-poly-roots-cubic-expression ()
  "x^3 - x^2 - 4*x + 4 -> [-2, 1, 2]."
  (my-calc-poly-roots-test "x^3 - x^2 - 4*x + 4" "[-2, 1, 2]"))

(ert-deftest test-my/calc-poly-roots-quadratic-expression ()
  "x^2 - 4 -> [-2, 2]."
  (my-calc-poly-roots-test "x^2 - 4" "[-2, 2]"))

(ert-deftest test-my/calc-poly-roots-linear-expression ()
  "x - 3 -> [3]."
  (my-calc-poly-roots-test "x - 3" "[3]"))


;;; Equation forms

(ert-deftest test-my/calc-poly-roots-cubic-equation ()
  "x^3 - x^2 - 4*x + 4 = 0 -> [-2, 1, 2]."
  (my-calc-poly-roots-test "x^3 - x^2 - 4*x + 4 = 0" "[-2, 1, 2]"))

(ert-deftest test-my/calc-poly-roots-quadratic-equation ()
  "x^2 - 4 = 0 -> [-2, 2]."
  (my-calc-poly-roots-test "x^2 - 4 = 0" "[-2, 2]"))


;;; Factored forms

(ert-deftest test-my/calc-poly-roots-factored-expression ()
  "(x + 2)(x - 1)(x - 2) -> [-2, 1, 2]."
  (my-calc-poly-roots-test "(x + 2) * (x - 1) * (x - 2)" "[-2, 1, 2]"))

(ert-deftest test-my/calc-poly-roots-factored-equation ()
  "(x + 2)(x - 1)(x - 2) = 0 -> [-2, 1, 2]."
  (my-calc-poly-roots-test "(x + 2) * (x - 1) * (x - 2) = 0" "[-2, 1, 2]"))


;;; Repeated roots (multiplicity preserved)

(ert-deftest test-my/calc-poly-roots-repeated-root ()
  "(x - 2)^2 -> [2, 2] (multiplicity shown)."
  (my-calc-poly-roots-test "(x - 2)^2" "[2, 2]"))

(ert-deftest test-my/calc-poly-roots-triple-root ()
  "(x - 1)^3 -> [1, 1, 1]."
  (my-calc-poly-roots-test "(x - 1)^3" "[1, 1, 1]"))

(ert-deftest test-my/calc-poly-roots-repeated-with-simple ()
  "(x - 1)^2 * (x + 2) -> [-2, 1, 1]."
  (my-calc-poly-roots-test "(x - 1)^2 * (x + 2)" "[-2, 1, 1]"))

(ert-deftest test-my/calc-poly-roots-repeated-expanded ()
  "x^3 - 3*x^2 + 3*x - 1 -> [1, 1, 1]."
  (my-calc-poly-roots-test "x^3 - 3*x^2 + 3*x - 1" "[1, 1, 1]"))


;;; Function definition forms f(x) = ...

(ert-deftest test-my/calc-poly-roots-f-quadratic ()
  "f(x) = x^2 - 4 -> [-2, 2]."
  (my-calc-poly-roots-test "f(x) = x^2 - 4" "[-2, 2]"))

(ert-deftest test-my/calc-poly-roots-g-linear ()
  "g(x) = x - 3 -> [3]."
  (my-calc-poly-roots-test "g(x) = x - 3" "[3]"))

(ert-deftest test-my/calc-poly-roots-p-cubic ()
  "p(x) = x^3 - x^2 - 4*x + 4 -> [-2, 1, 2]."
  (my-calc-poly-roots-test "p(x) = x^3 - x^2 - 4*x + 4" "[-2, 1, 2]"))


;;; Transcendental forms

(ert-deftest test-my/calc-poly-roots-log10 ()
  "log10(-x + 1) -> [0, 0]."
  (my-calc-poly-roots-test "log10(-x + 1)" "[0, 0]"))


(provide 'my-calc-poly-roots-tests)
