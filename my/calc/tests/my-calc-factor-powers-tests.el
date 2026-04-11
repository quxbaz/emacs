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
    (math-zerop (math-simplify diff))))

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

;;; Sum/difference of sixth powers with divided coefficient

(ert-deftest test-my/calc-factor-powers-a6-minus-b6-over-8 ()
  "a^6 - b^6/8 factors as (a^2 - b^2/2)(a^4 + a^2*b^2/2 + b^4/4)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "a^6 - b^6/8")))
      (should (my-calc-factor-powers-tests--equiv
               result "(a^2 - b^2/2)*(a^4 + a^2*b^2/2 + b^4/4)")))))

(ert-deftest test-my/calc-factor-powers-a6-plus-b6-over-8 ()
  "a^6 + b^6/8 factors as (a^2 + b^2/2)(a^4 - a^2*b^2/2 + b^4/4)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "a^6 + b^6/8")))
      (should (my-calc-factor-powers-tests--equiv
               result "(a^2 + b^2/2)*(a^4 - a^2*b^2/2 + b^4/4)")))))

(ert-deftest test-my/calc-factor-powers-a6-minus-b6-over-27 ()
  "a^6 - b^6/27 factors using cube root of 27 = 3."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "a^6 - b^6/27")))
      (should (my-calc-factor-powers-tests--equiv
               result "(a^2 - b^2/3)*(a^4 + a^2*b^2/3 + b^4/9)")))))

;;; Sum/difference with divided coefficients on both sides

(ert-deftest test-my/calc-factor-powers-a6-over-8-minus-b3-over-8 ()
  "a^6/8 - b^3/8 factors as (a^2/2 - b/2)(a^4/4 + a^2*b/4 + b^2/4)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "a^6/8 - b^3/8")))
      (should (my-calc-factor-powers-tests--equiv
               result "(a^2/2 - b/2)*(a^4/4 + a^2*b/4 + b^2/4)")))))

(ert-deftest test-my/calc-factor-powers-a6-over-8-plus-b3-over-8 ()
  "a^6/8 + b^3/8 factors as (a^2/2 + b/2)(a^4/4 - a^2*b/4 + b^2/4)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "a^6/8 + b^3/8")))
      (should (my-calc-factor-powers-tests--equiv
               result "(a^2/2 + b/2)*(a^4/4 - a^2*b/4 + b^2/4)")))))

(ert-deftest test-my/calc-factor-powers-a6-over-27-minus-b6-over-8 ()
  "a^6/27 - b^6/8 factors with different cube-root denominators."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "a^6/27 - b^6/8")))
      (should (my-calc-factor-powers-tests--equiv
               result "(a^2/3 - b^2/2)*(a^4/9 + a^2*b^2/6 + b^4/4)")))))

;;; Divided coefficient on left only

(ert-deftest test-my/calc-factor-powers-a6-over-8-minus-b3 ()
  "a^6/8 - b^3 factors as (a^2/2 - b)(a^4/4 + a^2*b/2 + b^2)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "a^6/8 - b^3")))
      (should (my-calc-factor-powers-tests--equiv
               result "(a^2/2 - b)*(a^4/4 + a^2*b/2 + b^2)")))))

(ert-deftest test-my/calc-factor-powers-a6-over-8-plus-b3 ()
  "a^6/8 + b^3 factors as (a^2/2 + b)(a^4/4 - a^2*b/2 + b^2)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-factor-powers-tests--factor "a^6/8 + b^3")))
      (should (my-calc-factor-powers-tests--equiv
               result "(a^2/2 + b)*(a^4/4 - a^2*b/2 + b^2)")))))

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
