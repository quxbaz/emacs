;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-exp

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

;;; Basic stack tests

(ert-deftest test-my/calc-exp-basic ()
  "exp applied to top of stack."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))
    (my/calc-exp)
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "exp(x)")))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-exp-exp10 ()
  "H E applies 10^ instead of exp."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))
    (let ((calc-hyperbolic-flag t))
      (my/calc-exp))
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "10^x")))))
      (should (math-zerop (math-simplify diff))))))

;;; Selection test

(ert-deftest test-my/calc-exp-selection ()
  "exp applied to selected sub-expression only."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "y + x"))
           (sel-expr  (nth 2 full-expr)))  ; x
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (my/calc-exp)
      (let* ((result (car (nth 1 calc-stack)))
             (diff (math-normalize
                    (list '- result (math-read-expr "y + exp(x)")))))
        (should (math-zerop (math-simplify diff)))))))

;;; Equation-map test (at EOL)

(ert-deftest test-my/calc-exp-equation-at-eol ()
  "At eol on an equation, exp is applied to both sides independently."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x = y"))
    (calc-cursor-stack-index 1)
    (end-of-line)
    (my/calc-exp)
    (let* ((result (car (nth 1 calc-stack)))
           (lhs-diff (math-normalize
                      (list '- (nth 1 result) (math-read-expr "exp(x)"))))
           (rhs-diff (math-normalize
                      (list '- (nth 2 result) (math-read-expr "exp(y)")))))
      (should (eq (car-safe result) 'calcFunc-eq))
      (should (math-zerop (math-simplify lhs-diff)))
      (should (math-zerop (math-simplify rhs-diff))))))

(provide 'my-calc-exp-tests)
