;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-ln

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

;;; Basic stack tests

(ert-deftest test-my/calc-ln-basic ()
  "ln applied to top of stack."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))
    (my/calc-ln)
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "ln(x)")))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-ln-log10 ()
  "H L applies log10 instead of ln."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))
    (let ((calc-hyperbolic-flag t))
      (my/calc-ln))
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "log10(x)")))))
      (should (math-zerop (math-simplify diff))))))

;;; Selection test

(ert-deftest test-my/calc-ln-selection ()
  "ln applied to selected sub-expression only."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "y + x"))
           (sel-expr  (nth 2 full-expr)))  ; x
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (my/calc-ln)
      (let* ((result (car (nth 1 calc-stack)))
             (diff (math-normalize
                    (list '- result (math-read-expr "y + ln(x)")))))
        (should (math-zerop (math-simplify diff)))))))

;;; Equation-map test (at EOL)

(ert-deftest test-my/calc-ln-equation-at-eol ()
  "At eol on an equation, ln is applied to both sides independently."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x = y"))
    (calc-cursor-stack-index 1)
    (end-of-line)
    (my/calc-ln)
    (let* ((result (car (nth 1 calc-stack)))
           (lhs-diff (math-normalize
                      (list '- (nth 1 result) (math-read-expr "ln(x)"))))
           (rhs-diff (math-normalize
                      (list '- (nth 2 result) (math-read-expr "ln(y)")))))
      (should (eq (car-safe result) 'calcFunc-eq))
      (should (math-zerop (math-simplify lhs-diff)))
      (should (math-zerop (math-simplify rhs-diff))))))

(provide 'my-calc-ln-tests)
