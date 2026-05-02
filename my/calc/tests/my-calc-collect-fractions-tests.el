;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-collect-fractions
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
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

;;; Selection tests

(ert-deftest test-my/calc-collect-fractions-respects-selection ()
  "my/calc-collect-fractions operates only on the selected sub-expression.
With (x/2 + y/3) + 1 on the stack and x/2 + y/3 selected, the result
should be (3x + 2y)/6 + 1."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "(x/2 + y/3) + 1"))
           (sel-expr  (nth 1 full-expr)))   ; must be eq to the sub-expr in the entry
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (my/calc-collect-fractions)
      (let* ((result (car (nth 1 calc-stack)))
             (diff   (math-normalize
                      (list '- result (math-read-expr "(3*x + 2*y)/6 + 1")))))
        (should (math-zerop (math-simplify diff)))))))

(ert-deftest test-my/calc-collect-fractions-selection-in-nested-expr ()
  "With x = 2^(y/3 - 1:3) + 1 on stack and (y/3 - 1:3) selected,
result should be x = 2^((y - 1)/3) + 1."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "x = 2^(y/3 - 1:3) + 1"))
           ;; Navigate to the exponent: full-expr[2][1][2]
           ;; calcFunc-eq[x, (+[^[2, -[y/3, 1:3]], 1])]
           (sel-expr (nth 2 (nth 1 (nth 2 full-expr)))))
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (my/calc-collect-fractions)
      (let* ((result (car (nth 1 calc-stack)))
             (diff   (math-normalize
                      (list '- result (math-read-expr "x = 2^((y - 1)/3) + 1")))))
        (should (math-zerop (math-simplify diff)))))))

(ert-deftest test-my/calc-collect-fractions-selection-exponent ()
  "With 2^(y/3 - 1:3) on stack and (y/3 - 1:3) selected,
result should be 2^((y - 1)/3)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "2^(y/3 - 1:3)"))
           (sel-expr  (nth 2 full-expr)))   ; the exponent: y/3 - 1:3
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (my/calc-collect-fractions)
      (let* ((result (car (nth 1 calc-stack)))
             (diff   (math-normalize
                      (list '- result (math-read-expr "2^((y - 1)/3)")))))
        (should (math-zerop (math-simplify diff)))))))

(provide 'my-calc-collect-fractions-tests)
