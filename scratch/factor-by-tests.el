;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-factor-by and my/calc-apply-sel-or-top
;;

(require 'ert)
(require 'calc)
(require 'cl-lib)

;; Load the code under test
(load-file (expand-file-name "scratch/factor-by.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))

;;; Macro expansion tests

(ert-deftest test-my/calc-apply-sel-or-top-expands ()
  "Test that the macro expands without errors."
  (should (macroexpand '(my/calc-apply-sel-or-top (expr replace-expr) ((m 2) (prefix "test"))
                          (message "test")))))

(ert-deftest test-my/calc-apply-sel-or-top-generates-cl-flet ()
  "Test that the macro generates cl-flet, not fset."
  (let ((expansion (macroexpand '(my/calc-apply-sel-or-top (expr replace-expr) ((m 2))
                                   (replace-expr 'foo)))))
    ;; Should contain cl-flet
    (should (string-match-p "cl-flet" (format "%S" expansion)))
    ;; Should NOT contain fset
    (should-not (string-match-p "fset" (format "%S" expansion)))))

;;; Basic calc stack tests (without selections)

(ert-deftest test-my/calc-factor-by-simple ()
  "Test factoring 2x + 4 by 2 yields 2 * (x + 2)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    ;; Push 2x + 4 directly
    (calc-push (math-read-expr "2*x + 4"))
    ;; Push 2 (the factor)
    (calc-push 2)
    ;; Now stack has: 2x + 4, 2
    ;; Factor by 2
    (my/calc-factor-by)
    ;; Result should be 2 * (x + 2) or equivalent
    (let ((result (calc-top-n 1)))
      (should (equal (math-format-value result) "2 (x + 2)")))))

(ert-deftest test-my/calc-factor-by-numeric ()
  "Test factoring 6 by 3 yields 3 * 2."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    ;; Push 6
    (calc-push 6)
    ;; Push 3 (the factor)
    (calc-push 3)
    ;; Factor by 3
    (my/calc-factor-by)
    ;; Result should be 3 * 2 = 6 or just 2 depending on simplification
    (let ((result (calc-top-n 1)))
      ;; Should be 3 * 2 or 6
      (should (or (equal result 6)
                  (equal (math-format-value result) "3 * 2"))))))

(ert-deftest test-my/calc-factor-by-polynomial ()
  "Test factoring x^2 + 2x by x yields x * (x + 2)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    ;; Push x^2 + 2x
    (calc-push (math-read-expr "x^2 + 2*x"))
    ;; Push x (the factor)
    (calc-push (math-read-expr "x"))
    ;; Factor by x
    (my/calc-factor-by)
    ;; Result should be x * (x + 2)
    (let ((result (calc-top-n 1)))
      (should (string-match-p "x.*x.*\\+" (math-format-value result))))))

;;; Edge cases

(ert-deftest test-my/calc-apply-sel-or-top-with-default-m ()
  "Test that m defaults to 1 when not specified."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 10)
    (my/calc-apply-sel-or-top (expr replace-expr) ((prefix "test"))
      ;; expr should be the top of stack
      (should (equal expr 10)))))

(ert-deftest test-my/calc-apply-sel-or-top-with-custom-m ()
  "Test that custom m value is respected."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 10)  ; Stack level 2
    (calc-push 20)  ; Stack level 1 (top)
    (my/calc-apply-sel-or-top (expr replace-expr) ((m 2))
      ;; expr should be stack level 2 (which is 10)
      (should (equal expr 10)))))

(ert-deftest test-my/calc-apply-sel-or-top-binds-sel-is-active ()
  "Test that sel-is-active is bound correctly."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 42)
    (my/calc-apply-sel-or-top (expr replace-expr sel-is-active) ()
      ;; sel-is-active should be nil (no selections)
      (should-not sel-is-active))))

(ert-deftest test-my/calc-apply-sel-or-top-replace-expr-callable ()
  "Test that replace-expr works as a function."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 10)
    (my/calc-apply-sel-or-top (expr replace-expr) ((prefix "test"))
      ;; Call replace-expr to replace with 99
      ;; (Note: replace-expr is a cl-flet binding, not a variable, so functionp won't work)
      (replace-expr 99)
      ;; Top of stack should now be 99
      (should (equal (calc-top-n 1) 99)))))

;;; Gensym tests

(ert-deftest test-my/calc-apply-sel-or-top-unused-bindings-gensym ()
  "Test that unused bindings are auto-gensymmed."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 42)
    ;; Only bind expr, let others be gensymmed
    (my/calc-apply-sel-or-top (expr) ()
      (should (equal expr 42)))))

(ert-deftest test-my/calc-apply-sel-or-top-no-bindings-gensym ()
  "Test that completely empty bindings use gensyms."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 42)
    ;; All bindings gensymmed
    (my/calc-apply-sel-or-top () ()
      ;; Should still work, just can't reference the values
      (should (equal (calc-top-n 1) 42)))))

(provide 'factor-by-tests)
