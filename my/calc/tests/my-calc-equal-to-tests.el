;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-equal-to

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))


;;; Helper

(defmacro calc-equal-to-test (a b inverse-p &rest body)
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,a))
     (calc-push (math-read-expr ,b))
     (let ((calc-inverse-flag ,inverse-p))
       (my/calc-equal-to nil))
     ,@body))


;;; No simplification

(ert-deftest test-calc-equal-to-no-simplify-lhs ()
  "LHS expression is not distributed/simplified: 2*(3+x) stays as a product."
  (calc-equal-to-test "2 * (3 + x)" "y" nil
    (let ((lhs (nth 1 (car (nth 1 calc-stack)))))
      ;; Should be (* 2 ...), not (+ (* 2 x) 6)
      (should (eq (car-safe lhs) '*)))))

(ert-deftest test-calc-equal-to-no-simplify-rhs ()
  "RHS fraction 1/9*y is not converted to y/9."
  (calc-equal-to-test "x" "1:9 * y" nil
    (let ((rhs (nth 2 (car (nth 1 calc-stack)))))
      ;; Should be (* (frac 1 9) ...), not (/ y 9)
      (should (eq (car-safe rhs) '*)))))


;;; Equal-to (no inverse)

(ert-deftest test-calc-equal-to-produces-eq ()
  "Without inverse flag, produces a calcFunc-eq expression."
  (calc-equal-to-test "x" "y" nil
    (should (eq (car-safe (car (nth 1 calc-stack))) 'calcFunc-eq))))

(ert-deftest test-calc-equal-to-stack-reduced ()
  "Two items consumed, one equation left on stack."
  (calc-equal-to-test "x" "y" nil
    (should (= (calc-stack-size) 1))))

(ert-deftest test-calc-equal-to-operands ()
  "LHS and RHS match the pushed values."
  (calc-equal-to-test "x" "y" nil
    (let ((result (car (nth 1 calc-stack))))
      (should (equal (math-format-value (nth 1 result))
                     (math-format-value (math-read-expr "x"))))
      (should (equal (math-format-value (nth 2 result))
                     (math-format-value (math-read-expr "y")))))))


;;; Not-equal-to (inverse)

(ert-deftest test-calc-equal-to-inverse-produces-neq ()
  "With inverse flag, produces a calcFunc-neq expression."
  (calc-equal-to-test "x" "y" t
    (should (eq (car-safe (car (nth 1 calc-stack))) 'calcFunc-neq))))

(ert-deftest test-calc-equal-to-inverse-stack-reduced ()
  "Two items consumed, one expression left on stack."
  (calc-equal-to-test "x" "y" t
    (should (= (calc-stack-size) 1))))

(ert-deftest test-calc-equal-to-inverse-operands ()
  "LHS and RHS of neq match the pushed values."
  (calc-equal-to-test "x" "y" t
    (let ((result (car (nth 1 calc-stack))))
      (should (equal (math-format-value (nth 1 result))
                     (math-format-value (math-read-expr "x"))))
      (should (equal (math-format-value (nth 2 result))
                     (math-format-value (math-read-expr "y")))))))
