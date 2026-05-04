;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-power

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

;;; Basic stack tests

(ert-deftest test-my/calc-power-basic ()
  "expr ^ exp: expr on level 2, exponent on level 1."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))  ; level 2 (base)
    (calc-push (math-read-expr "n"))  ; level 1 (exponent)
    (my/calc-power)
    (should (= (calc-stack-size) 1))
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "x^n")))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-power-exponent-consumed ()
  "After ^, the exponent is consumed from the stack — only one item remains."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))
    (calc-push (math-read-expr "n"))
    (my/calc-power)
    (should (= (calc-stack-size) 1))))

(ert-deftest test-my/calc-power-nroot ()
  "I ^: nroot(expr, n) = expr^(1/n)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x"))  ; level 2 (radicand)
    (calc-push (math-read-expr "n"))  ; level 1 (root degree)
    (let ((calc-inverse-flag t))
      (my/calc-power))
    (should (= (calc-stack-size) 1))
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-normalize
                  (list '- result (math-read-expr "x^(1/n)")))))
      (should (math-zerop (math-simplify diff))))))

;;; Numeric inputs

(ert-deftest test-my/calc-power-numeric-exact ()
  "2^10 = 1024."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 2)
    (calc-push 10)
    (my/calc-power)
    (should (equal (car (nth 1 calc-stack)) 1024))))

(ert-deftest test-my/calc-power-nroot-numeric ()
  "I ^ on 8, 3: nroot(8, 3) = 2."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 8)
    (calc-push 3)
    (let ((calc-inverse-flag t))
      (my/calc-power))
    (should (equal (car (nth 1 calc-stack)) 2))))

;;; Selection test

(ert-deftest test-my/calc-power-selection ()
  "^ applied to selected sub-expression; exponent consumed from stack top."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "y + x"))
           (sel-expr  (nth 2 full-expr)))  ; x
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (calc-push (math-read-expr "n"))
      (my/calc-power)
      (let* ((result (car (nth 1 calc-stack)))
             (diff (math-normalize
                    (list '- result (math-read-expr "y + x^n")))))
        (should (math-zerop (math-simplify diff)))))))

;;; Equation-map test (at EOL)

(ert-deftest test-my/calc-power-equation-at-eol ()
  "At eol on an equation, ^ is applied to both sides; exponent consumed once."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x = y"))
    (calc-push (math-read-expr "n"))
    (calc-cursor-stack-index 2)
    (end-of-line)
    (my/calc-power)
    (should (= (calc-stack-size) 1))
    (let* ((result (car (nth 1 calc-stack)))
           (lhs-diff (math-normalize
                      (list '- (nth 1 result) (math-read-expr "x^n"))))
           (rhs-diff (math-normalize
                      (list '- (nth 2 result) (math-read-expr "y^n")))))
      (should (eq (car-safe result) 'calcFunc-eq))
      (should (math-zerop (math-simplify lhs-diff)))
      (should (math-zerop (math-simplify rhs-diff))))))

(provide 'my-calc-power-tests)
