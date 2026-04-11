;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-factor-powers
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 's)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/rewrite.el" user-emacs-directory))

(defun my-calc-factor-powers-tests--factor (input)
  "Push INPUT (calc expression string) onto the stack and factor it.
Returns the resulting top-of-stack expression."
  (calc-reset 0)
  (calc-push (math-read-expr input))
  (my/calc-factor-powers)
  (car (nth 1 calc-stack)))

(defun my-calc-factor-powers-tests--equiv (actual expected-str)
  "Return t if calc expr ACTUAL is mathematically equal to EXPECTED-STR.
Compares by expanding (actual - expected) and simplifying to 0."
  (let* ((expected (math-read-expr expected-str))
         (diff (math-normalize (list 'calcFunc-expand (list '- actual expected)))))
    (equal (math-simplify diff) 0)))

;;; Sum/difference of sixth powers (cubes of squares)

(ert-deftest test-my/calc-factor-powers-x6-minus-y6 ()
  "x^6 - y^6 factors as (x^2 - y^2)(x^4 + x^2*y^2 + y^4)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "x^6 - y^6")))
      (should (my-calc-factor-powers-tests--equiv
               result "(x^2 - y^2)*(x^4 + x^2*y^2 + y^4)")))))

(ert-deftest test-my/calc-factor-powers-x6-plus-y6 ()
  "x^6 + y^6 factors as (x^2 + y^2)(x^4 - x^2*y^2 + y^4)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "x^6 + y^6")))
      (should (my-calc-factor-powers-tests--equiv
               result "(x^2 + y^2)*(x^4 - x^2*y^2 + y^4)")))))

(ert-deftest test-my/calc-factor-powers-x6-minus-y6-is-factored ()
  "x^6 - y^6 result is a product (factoring actually occurred)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "x^6 - y^6")))
      (should (eq (car-safe result) '*)))))

(ert-deftest test-my/calc-factor-powers-x6-plus-y6-is-factored ()
  "x^6 + y^6 result is a product (factoring actually occurred)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "x^6 + y^6")))
      (should (eq (car-safe result) '*)))))

;;; Mixed multiples of 3 exponents

(ert-deftest test-my/calc-factor-powers-x9-minus-y9 ()
  "x^9 - y^9 factors via difference of cubes."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "x^9 - y^9")))
      (should (my-calc-factor-powers-tests--equiv
               result "(x^3 - y^3)*(x^6 + x^3*y^3 + y^6)")))))

(ert-deftest test-my/calc-factor-powers-x9-plus-y9 ()
  "x^9 + y^9 factors via sum of cubes."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "x^9 + y^9")))
      (should (my-calc-factor-powers-tests--equiv
               result "(x^3 + y^3)*(x^6 - x^3*y^3 + y^6)")))))

;;; Regression: existing cube rules still work

(ert-deftest test-my/calc-factor-powers-x3-minus-y3 ()
  "x^3 - y^3 still factors as (x - y)(x^2 + x*y + y^2)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "x^3 - y^3")))
      (should (my-calc-factor-powers-tests--equiv
               result "(x - y)*(x^2 + x*y + y^2)")))))

(ert-deftest test-my/calc-factor-powers-x3-plus-y3 ()
  "x^3 + y^3 still factors as (x + y)(x^2 - x*y + y^2)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "x^3 + y^3")))
      (should (my-calc-factor-powers-tests--equiv
               result "(x + y)*(x^2 - x*y + y^2)")))))

(provide 'my-calc-factor-powers-tests)
