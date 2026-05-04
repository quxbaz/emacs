;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-log

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

;;; Basic stack tests

(ert-deftest test-my/calc-log-basic ()
  "log(x, b): x on level 2, base b on level 1."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))  ; level 2
    (calc-push (math-read-expr "b"))  ; level 1 (base)
    (my/calc-log)
    (should (= (calc-stack-size) 1))
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "log(x, b)")))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-log-alog ()
  "I B: alog(base, expr) = base^expr."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))  ; level 2 (exponent)
    (calc-push (math-read-expr "b"))  ; level 1 (base)
    (let ((calc-inverse-flag t))
      (my/calc-log))
    (should (= (calc-stack-size) 1))
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "b^x")))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-log-base-consumed ()
  "After log, the base is consumed from the stack — only one item remains."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))
    (calc-push (math-read-expr "b"))
    (my/calc-log)
    (should (= (calc-stack-size) 1))))

;;; Selection test

(ert-deftest test-my/calc-log-selection ()
  "log applied to selected sub-expression; base consumed from stack top."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "y + x"))
           (sel-expr  (nth 2 full-expr)))  ; x
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (calc-push (math-read-expr "b"))
      (my/calc-log)
      (let* ((result (car (nth 1 calc-stack)))
             (diff (math-normalize
                    (list '- result (math-read-expr "y + log(x, b)")))))
        (should (math-zerop (math-simplify diff)))))))

;;; Equation-map test (at EOL)

(ert-deftest test-my/calc-log-equation-at-eol ()
  "At eol on an equation, log is applied to both sides; base consumed once."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x = y"))
    (calc-push (math-read-expr "b"))
    (calc-cursor-stack-index 2)
    (end-of-line)
    (my/calc-log)
    (should (= (calc-stack-size) 1))
    (let* ((result (car (nth 1 calc-stack)))
           (lhs-diff (math-normalize
                      (list '- (nth 1 result) (math-read-expr "log(x, b)"))))
           (rhs-diff (math-normalize
                      (list '- (nth 2 result) (math-read-expr "log(y, b)")))))
      (should (eq (car-safe result) 'calcFunc-eq))
      (should (math-zerop (math-simplify lhs-diff)))
      (should (math-zerop (math-simplify rhs-diff))))))

;;; Numeric inputs — these would throw inexact-result without calc-normalize

(ert-deftest test-my/calc-log-numeric-exact ()
  "log(8, 2) = 3 (exact integer result)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 8)
    (calc-push 2)
    (my/calc-log)
    (should (equal (car (nth 1 calc-stack)) 3))))

(ert-deftest test-my/calc-log-numeric-irrational ()
  "log(5, 2) is irrational — must not throw inexact-result, stays symbolic."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 5)
    (calc-push 2)
    (should-not (condition-case _ (progn (my/calc-log) nil) (error t)))
    (should (= (calc-stack-size) 1))))

(ert-deftest test-my/calc-alog-numeric ()
  "alog(3, 2) = 2^3 = 8."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 3)   ; exponent
    (calc-push 2)   ; base
    (let ((calc-inverse-flag t))
      (my/calc-log))
    (should (equal (car (nth 1 calc-stack)) 8))))

(provide 'my-calc-log-tests)
