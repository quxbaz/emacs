;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-collect-fractions
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/rewrite.el" user-emacs-directory))

(defun my-calc-collect-fractions-tests--run (input-str)
  "Push INPUT-STR, run my/calc-collect-fractions, return top of stack."
  (calc-reset 0)
  (calc-push (math-read-expr input-str))
  (my/calc-collect-fractions)
  (car (nth 1 calc-stack)))

(defun my-calc-collect-fractions-tests--single-fraction-p (expr)
  "Return t if EXPR is a single division (not a sum/difference)."
  (and (eq (car-safe expr) '/)
       (not (memq (car-safe (nth 1 expr)) '(+ -)))))

;;; Basic same-denominator tests

(ert-deftest test-my/calc-collect-fractions-pi-half-minus-half ()
  "pi/2 - 1/2 -> (pi - 1)/2."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-collect-fractions-tests--run "pi/2 - 1/2"))
           (diff (math-normalize
                  (list 'calcFunc-expand (list '- result (math-read-expr "(pi - 1)/2"))))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-collect-fractions-pi-half-minus-half-is-fraction ()
  "pi/2 - 1/2 result is a single fraction."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-collect-fractions-tests--run "pi/2 - 1/2")))
      (should (eq (car-safe result) '/)))))

(ert-deftest test-my/calc-collect-fractions-same-denom-add ()
  "x/3 + 2/3 -> (x + 2)/3."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-collect-fractions-tests--run "x/3 + 2/3"))
           (diff (math-normalize
                  (list 'calcFunc-expand (list '- result (math-read-expr "(x + 2)/3"))))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-collect-fractions-different-denoms ()
  "x/2 + x/3 -> 5x/6."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-collect-fractions-tests--run "x/2 + x/3"))
           (diff (math-normalize
                  (list 'calcFunc-expand (list '- result (math-read-expr "5*x/6"))))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-collect-fractions-three-terms ()
  "a/6 + b/3 - c/2 -> (a + 2*b - 3*c)/6."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-collect-fractions-tests--run "a/6 + b/3 - c/2"))
           (diff (math-normalize
                  (list 'calcFunc-expand (list '- result (math-read-expr "(a + 2*b - 3*c)/6"))))))
      (should (math-zerop (math-simplify diff))))))

;;; Fraction divided by symbolic expression

(ert-deftest test-my/calc-collect-fractions-frac-over-power ()
  "8:3 / x^2 -> 8 / (3 x^2): integer numerator, denom absorbs fraction denominator."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-collect-fractions-tests--run "8:3 / x^2")))
      (should (eq (car-safe result) '/))
      (should (equal (nth 1 result) 8))
      (let ((diff (math-normalize
                   (list '- result (math-read-expr "8 / (3 * x^2)")))))
        (should (math-zerop (math-simplify diff)))))))

(ert-deftest test-my/calc-collect-fractions-frac-over-binomial-power ()
  "8:3 / (x+1)^2 -> 8 / (3 (x+1)^2): denom absorbs fraction denominator."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-collect-fractions-tests--run "8:3 / (x + 1)^2")))
      (should (eq (car-safe result) '/))
      (should (equal (nth 1 result) 8))
      (let ((diff (math-normalize
                   (list '- result (math-read-expr "8 / (3 * (x + 1)^2)")))))
        (should (math-zerop (math-simplify diff)))))))

(provide 'my-calc-collect-fractions-tests)
