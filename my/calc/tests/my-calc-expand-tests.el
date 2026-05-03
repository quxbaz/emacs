;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-expand

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

;;; Basic stack tests

(ert-deftest test-my/calc-expand-binomial ()
  "(x+1)^2 expands to x^2 + 2x + 1."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "(x+1)^2"))
    (my/calc-expand nil)
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "x^2 + 2*x + 1")))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-expand-product ()
  "(x+1)*(x+2) expands to x^2 + 3x + 2."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "(x+1)*(x+2)"))
    (my/calc-expand nil)
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "x^2 + 3*x + 2")))))
      (should (math-zerop (math-simplify diff))))))

;;; Selection test

(ert-deftest test-my/calc-expand-selection ()
  "Expands only the selected sub-expression.
In y + (x+1)^2, expand the selected (x+1)^2 to get y + x^2+2x+1."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "y + (x+1)^2"))
           (sel-expr  (nth 2 full-expr)))  ; (x+1)^2
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (my/calc-expand nil)
      (let* ((result (car (nth 1 calc-stack)))
             (diff (math-normalize
                    (list '- result (math-read-expr "y + x^2 + 2*x + 1")))))
        (should (math-zerop (math-simplify diff)))))))

;;; Equation-map test (at EOL)

(ert-deftest test-my/calc-expand-equation-at-eol ()
  "At eol on an equation, expands both sides independently.
(x+1)^2 = (x+2)^2 → x^2+2x+1 = x^2+4x+4."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "(x+1)^2 = (x+2)^2"))
    (calc-cursor-stack-index 1)
    (end-of-line)
    (my/calc-expand nil)
    (let* ((result (car (nth 1 calc-stack)))
           (lhs-diff (math-normalize
                      (list '- (nth 1 result)
                            (math-read-expr "x^2 + 2*x + 1"))))
           (rhs-diff (math-normalize
                      (list '- (nth 2 result)
                            (math-read-expr "x^2 + 4*x + 4")))))
      (should (eq (car-safe result) 'calcFunc-eq))
      (should (math-zerop (math-simplify lhs-diff)))
      (should (math-zerop (math-simplify rhs-diff))))))

(provide 'my-calc-expand-tests)
