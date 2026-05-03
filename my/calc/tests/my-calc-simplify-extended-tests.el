;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-simplify-extended

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

;;; Basic stack tests

(ert-deftest test-my/calc-simplify-extended-arcsin-sin ()
  "arcsin(sin(x)) simplifies to x (requires math-living-dangerously)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "arcsin(sin(x))"))
    (my/calc-simplify-extended)
    (should (equal (car (nth 1 calc-stack)) '(var x var-x)))))

(ert-deftest test-my/calc-simplify-extended-exp-ln ()
  "exp(ln(x)) simplifies to x (requires math-living-dangerously)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "exp(ln(x))"))
    (my/calc-simplify-extended)
    (should (equal (car (nth 1 calc-stack)) '(var x var-x)))))

(ert-deftest test-my/calc-simplify-extended-ln-exp ()
  "ln(exp(x)) simplifies to x (requires math-living-dangerously)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "ln(exp(x))"))
    (my/calc-simplify-extended)
    (should (equal (car (nth 1 calc-stack)) '(var x var-x)))))

;;; Selection test

(ert-deftest test-my/calc-simplify-extended-selection ()
  "Simplifies the selected sub-expression, leaving the rest unchanged.
In x + exp(ln(y)), simplify the selected exp(ln(y)) to y,
yielding x + y."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "x + exp(ln(y))"))
           (sel-expr  (nth 2 full-expr)))  ; exp(ln(y))
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (my/calc-simplify-extended)
      (let* ((result (car (nth 1 calc-stack)))
             (diff (math-normalize
                    (list '- result (math-read-expr "x + y")))))
        (should (math-zerop (math-simplify diff)))))))

;;; Equation-map test (at EOL)

(ert-deftest test-my/calc-simplify-extended-equation-at-eol ()
  "At eol on an equation, simplifies both sides independently.
arcsin(sin(x)) = arctan(tan(y)) → x = y."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "arcsin(sin(x)) = arctan(tan(y))"))
    (calc-cursor-stack-index 1)
    (end-of-line)
    (my/calc-simplify-extended)
    (let ((result (car (nth 1 calc-stack))))
      (should (eq (car-safe result) 'calcFunc-eq))
      (should (equal (nth 1 result) '(var x var-x)))
      (should (equal (nth 2 result) '(var y var-y))))))

(provide 'my-calc-simplify-extended-tests)
