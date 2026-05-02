;; -*- lexical-binding: t; -*-
;;
;; Tests for DistribRules extended to handle frac (a:b) inside log/ln/log10

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/selection.el" user-emacs-directory))

(defun my-calc-distrib-frac-tests--run (input-str sel-nav-fn)
  "Push INPUT-STR, select via SEL-NAV-FN, run calc-sel-distribute, return top."
  (calc-reset 0)
  (let* ((full-expr (math-read-expr input-str))
         (sel-expr  (funcall sel-nav-fn full-expr)))
    (calc-push full-expr)
    (setf (nth 2 (nth 1 calc-stack)) sel-expr)
    (setq calc-use-selections t)
    (my/calc-sel-distribute)
    (car (nth 1 calc-stack))))

;;; ln tests

(ert-deftest test-my/calc-distrib-frac-ln-3:2 ()
  "ln(3:2) with 3:2 selected -> ln(3) - ln(2)."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-distrib-frac-tests--run
                    "ln(3:2)"
                    (lambda (e) (nth 1 e))))   ; the argument 3:2
           (diff (math-normalize
                  (list '- result (math-read-expr "ln(3) - ln(2)")))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc-distrib-frac-ln-1:3 ()
  "ln(1:3) with 1:3 selected -> ln(1) - ln(3)."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-distrib-frac-tests--run
                    "ln(1:3)"
                    (lambda (e) (nth 1 e))))
           (diff (math-normalize
                  (list '- result (math-read-expr "ln(1) - ln(3)")))))
      (should (math-zerop (math-simplify diff))))))

;;; log10 test

(ert-deftest test-my/calc-distrib-frac-log10-3:2 ()
  "log10(3:2) with 3:2 selected -> log10(3) - log10(2)."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-distrib-frac-tests--run
                    "log10(3:2)"
                    (lambda (e) (nth 1 e))))
           (diff (math-normalize
                  (list '- result (math-read-expr "log10(3) - log10(2)")))))
      (should (math-zerop (math-simplify diff))))))

;;; log(arg, base) test

(ert-deftest test-my/calc-distrib-frac-log-3:2-base2 ()
  "log(3:2, 2) with 3:2 selected -> log(3,2) - log(2,2)."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-distrib-frac-tests--run
                    "log(3:2, 2)"
                    (lambda (e) (nth 1 e))))   ; the first argument 3:2
           (diff (math-normalize
                  (list '- result (math-read-expr "log(3,2) - log(2,2)")))))
      (should (math-zerop (math-simplify diff))))))

;;; Regression: existing ln(a/b) still works

(ert-deftest test-my/calc-distrib-frac-ln-division-still-works ()
  "ln(x/y) with x/y selected still -> ln(x) - ln(y) (regression)."
  (with-temp-buffer
    (calc-mode)
    (let* ((result (my-calc-distrib-frac-tests--run
                    "ln(x/y)"
                    (lambda (e) (nth 1 e))))
           (diff (math-normalize
                  (list '- result (math-read-expr "ln(x) - ln(y)")))))
      (should (math-zerop (math-simplify diff))))))

(provide 'my-calc-sel-distribute-frac-tests)
