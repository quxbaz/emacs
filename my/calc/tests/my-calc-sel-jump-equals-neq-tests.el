;; -*- lexical-binding: t; -*-
;;
;; Tests for calc-sel-jump-equals extended with != (neq) support

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/selection.el" user-emacs-directory))

(ert-deftest test-my/calc-sel-jump-equals-neq-additive ()
  "jump-equals on 'a' in 'x + a != y' moves a to RHS: x != y - a."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    ;; full-expr: (calcFunc-neq (+ x a) y)
    ;; sel-expr: a, which is (nth 2 (nth 1 full-expr))
    (let* ((full-expr (math-read-expr "x + a != y"))
           (sel-expr  (nth 2 (nth 1 full-expr))))
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (calc-sel-jump-equals nil)
      (let* ((result (car (nth 1 calc-stack)))
             (diff   (math-normalize
                      (list '- result (math-read-expr "x != y - a")))))
        (should (eq (car-safe result) 'calcFunc-neq))
        (should (math-zerop (math-simplify diff)))))))

(ert-deftest test-my/calc-sel-jump-equals-neq-multiplicative ()
  "jump-equals on 'a' in 'a * x != y' moves a to RHS: a != y / x."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    ;; full-expr: (calcFunc-neq (* a x) y)
    ;; sel-expr: x, which is (nth 2 (nth 1 full-expr))
    (let* ((full-expr (math-read-expr "a * x != y"))
           (sel-expr  (nth 2 (nth 1 full-expr))))
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (calc-sel-jump-equals nil)
      (let* ((result (car (nth 1 calc-stack)))
             (diff   (math-normalize
                      (list '- result (math-read-expr "a != y / x")))))
        (should (eq (car-safe result) 'calcFunc-neq))
        (should (math-zerop (math-simplify diff)))))))

(provide 'my-calc-sel-jump-equals-neq-tests)
