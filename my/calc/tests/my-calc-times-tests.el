;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-times

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

;;; Basic stack tests

(ert-deftest test-my/calc-times-basic ()
  "expr * factor: expr on level 2, factor on level 1."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))  ; level 2
    (calc-push (math-read-expr "y"))  ; level 1 (multiplier)
    (my/calc-times)
    (should (= (calc-stack-size) 1))
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "x*y")))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-times-factor-consumed ()
  "After *, the multiplier is consumed — only one item remains."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))
    (calc-push (math-read-expr "y"))
    (my/calc-times)
    (should (= (calc-stack-size) 1))))

;;; Numeric inputs

(ert-deftest test-my/calc-times-numeric ()
  "3 * 4 = 12."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 3)
    (calc-push 4)
    (my/calc-times)
    (should (equal (car (nth 1 calc-stack)) 12))))

;;; Selection test

(ert-deftest test-my/calc-times-selection ()
  "* applied to selected sub-expression; multiplier consumed from stack top."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "y + x"))
           (sel-expr  (nth 2 full-expr)))  ; x
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (calc-push (math-read-expr "n"))
      (my/calc-times)
      (let* ((result (car (nth 1 calc-stack)))
             (diff (math-normalize
                    (list '- result (math-read-expr "y + x*n")))))
        (should (math-zerop (math-simplify diff)))))))

;;; Equation-map test (at EOL)

(ert-deftest test-my/calc-times-equation-at-eol ()
  "At eol on an equation, * is applied to both sides; multiplier consumed once."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x = y"))
    (calc-push (math-read-expr "n"))
    (calc-cursor-stack-index 2)
    (end-of-line)
    (my/calc-times)
    (should (= (calc-stack-size) 1))
    (let* ((result (car (nth 1 calc-stack)))
           (lhs-diff (math-normalize
                      (list '- (nth 1 result) (math-read-expr "x*n"))))
           (rhs-diff (math-normalize
                      (list '- (nth 2 result) (math-read-expr "y*n")))))
      (should (eq (car-safe result) 'calcFunc-eq))
      (should (math-zerop (math-simplify lhs-diff)))
      (should (math-zerop (math-simplify rhs-diff))))))

(provide 'my-calc-times-tests)
