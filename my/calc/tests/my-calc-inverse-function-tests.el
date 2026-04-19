;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-inverse-function
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))


;;; Helper

(defmacro my-calc-inverse-function-test (input expected)
  "Push INPUT, run my/calc-inverse-function, compare top-of-stack to EXPECTED."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,input))
     (my/calc-inverse-function)
     (should (equal (math-normalize (car (nth 1 calc-stack)))
                    (math-normalize (math-read-expr ,expected))))))


;;; y = f(x) equation forms

(ert-deftest test-my/calc-inverse-function-linear ()
  "y = x + 1 -> y = x - 1."
  (my-calc-inverse-function-test "y = x + 1" "y = x - 1"))

(ert-deftest test-my/calc-inverse-function-linear-scale ()
  "y = 2*x + 3 -> y = x/2 - 3/2."
  (my-calc-inverse-function-test "y = 2*x + 3" "y = x/2 - 3/2"))

(ert-deftest test-my/calc-inverse-function-quadratic ()
  "y = x^2 -> y = sqrt(x)."
  (my-calc-inverse-function-test "y = x^2" "y = sqrt(x)"))

(ert-deftest test-my/calc-inverse-function-sqrt ()
  "y = sqrt(x) -> y = x^2."
  (my-calc-inverse-function-test "y = sqrt(x)" "y = x^2"))


;;; f(x) = expr equation forms (retains f(x) on LHS)

(ert-deftest test-my/calc-inverse-function-fx-linear ()
  "f(x) = x + 1 -> f(x) = x - 1."
  (my-calc-inverse-function-test "f(x) = x + 1" "f(x) = x - 1"))

(ert-deftest test-my/calc-inverse-function-fx-quadratic ()
  "f(x) = x^2 -> f(x) = sqrt(x)."
  (my-calc-inverse-function-test "f(x) = x^2" "f(x) = sqrt(x)"))


;;; Bare f(x) expression forms (outputs y = ...)

(ert-deftest test-my/calc-inverse-function-expr-linear ()
  "x + 1 -> y = x - 1."
  (my-calc-inverse-function-test "x + 1" "y = x - 1"))

(ert-deftest test-my/calc-inverse-function-expr-quadratic ()
  "x^2 -> y = sqrt(x)."
  (my-calc-inverse-function-test "x^2" "y = sqrt(x)"))


(provide 'my-calc-inverse-function-tests)
