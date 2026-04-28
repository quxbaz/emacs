;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-edit-wrap-parens

(require 'ert)

(load-file (expand-file-name "my/calc/edit.el" user-emacs-directory))


;;; Helper

(defun my/test-wrap-n (text n)
  "In a temp buffer with TEXT (cursor at end), call wrap-parens N times."
  (with-temp-buffer
    (insert text)
    (dotimes (_ n)
      (my/calc-edit-wrap-parens))
    (buffer-string)))


;;; Basic wrapping

(ert-deftest test-wrap-parens-first-invoke ()
  "First invoke wraps the innermost term."
  (should (equal (my/test-wrap-n "pi+2" 1) "pi+(2)")))

(ert-deftest test-wrap-parens-expand ()
  "Second invoke expands to include all terms."
  (should (equal (my/test-wrap-n "pi+2" 2) "(pi+2)")))

(ert-deftest test-wrap-parens-subexpr ()
  "Scan crosses `*' (not a stop char), wrapping the full multiplicative term."
  (should (equal (my/test-wrap-n "a+b*c" 1) "a+(b*c)")))


;;; Whitespace: no leading space inside parens

(ert-deftest test-wrap-parens-no-leading-space ()
  "Scan stops at `='; leading space must end up outside the parens."
  (should (equal (my/test-wrap-n "= pi+2" 2) "= (pi+2)")))

(ert-deftest test-wrap-parens-expands-past-equals ()
  "Third invoke continues expansion past the `=' boundary."
  (should (equal (my/test-wrap-n "= pi+2" 3) "(= pi+2)")))


;;; Expansion after cursor move

(ert-deftest test-wrap-parens-expand-after-move ()
  "Expansion works when cursor moves away and returns to just after `)'."
  (should (equal
           (with-temp-buffer
             (insert "pi+2")
             (my/calc-edit-wrap-parens)   ; -> pi+(2), cursor after `)'
             (goto-char (point-min))       ; move away
             (goto-char (point-max))       ; come back (still after `)' since it's at end)
             (my/calc-edit-wrap-parens)   ; should expand, not no-op
             (buffer-string))
           "(pi+2)")))


;;; Function calls treated as atoms

(ert-deftest test-wrap-parens-sqrt-alone ()
  "Cursor after sqrt(3) wraps the whole call, not just the arg."
  (should (equal (my/test-wrap-n "sqrt(3)" 1) "(sqrt(3))")))

(ert-deftest test-wrap-parens-sqrt-after-operator ()
  "Cursor after sqrt(3) in a + sqrt(3) wraps just the call."
  (should (equal (my/test-wrap-n "a + sqrt(3)" 1) "a + (sqrt(3))")))

(ert-deftest test-wrap-parens-sqrt-after-division ()
  "Cursor after sqrt(3) in 27 / sqrt(3) wraps just the call."
  (should (equal (my/test-wrap-n "27 / sqrt(3)" 1) "27 / (sqrt(3))")))

(ert-deftest test-wrap-parens-sqrt-expand-to-full-expr ()
  "Second press on 27 / (sqrt(3)) expands to wrap the full expression."
  (should (equal (my/test-wrap-n "27 / sqrt(3)" 2) "(27 / sqrt(3))")))

(ert-deftest test-wrap-parens-func-alone ()
  "Cursor after f(x) wraps the whole call."
  (should (equal (my/test-wrap-n "f(x)" 1) "(f(x))")))

(ert-deftest test-wrap-parens-func-after-operator ()
  "Cursor after f(x) in a + f(x) wraps just the call."
  (should (equal (my/test-wrap-n "a + f(x)" 1) "a + (f(x))")))

(ert-deftest test-wrap-parens-func-expand ()
  "Second press expands from (f(x)) to wrap the full expression."
  (should (equal (my/test-wrap-n "a + f(x)" 2) "(a + f(x))")))


;;; Region wrapping

(ert-deftest test-wrap-parens-region ()
  "Active region is wrapped regardless of last-command."
  (should (equal
           (with-temp-buffer
             (insert "pi+2")
             (set-mark 1)
             (goto-char 3)
             (setq mark-active t)
             (my/calc-edit-wrap-parens)
             (buffer-string))
           "(pi)+2")))
