;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-point-is-in-line-prefix-p

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'calc-yank)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))


;;; Helper

(defmacro with-calc-stack (exprs &rest body)
  "Set up a calc buffer with EXPRS pushed onto the stack, then run BODY."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     ,@(mapcar (lambda (e) `(calc-push (math-read-expr ,e))) exprs)
     ,@body))


;;; Home / dot line

(ert-deftest test-my/calc-point-is-in-line-prefix-p-at-home ()
  "Point at home (. line) returns nil."
  (with-calc-stack ("x + 1")
    (goto-char (point-max))
    (should-not (my/calc-point-is-in-line-prefix-p))))

(ert-deftest test-my/calc-point-is-in-line-prefix-p-empty-stack ()
  "Empty stack returns nil."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should-not (my/calc-point-is-in-line-prefix-p))))


;;; In the prefix

(ert-deftest test-my/calc-point-is-in-line-prefix-p-at-bol ()
  "Point at beginning of a stack entry line returns t."
  (with-calc-stack ("x + 1")
    (calc-cursor-stack-index 1)
    (beginning-of-line)
    (should (my/calc-point-is-in-line-prefix-p))))

(ert-deftest test-my/calc-point-is-in-line-prefix-p-after-colon ()
  "Point after the colon (but before the formula) returns t."
  (with-calc-stack ("x + 1")
    (calc-cursor-stack-index 1)
    (beginning-of-line)
    (search-forward ": ")
    (backward-char)
    (should (my/calc-point-is-in-line-prefix-p))))


;;; On the formula

(ert-deftest test-my/calc-point-is-in-line-prefix-p-at-formula-start ()
  "Point at the first character of the formula returns nil."
  (with-calc-stack ("x + 1")
    (calc-cursor-stack-index 1)
    (beginning-of-line)
    (skip-chars-forward " 0-9: ")
    (should-not (my/calc-point-is-in-line-prefix-p))))

(ert-deftest test-my/calc-point-is-in-line-prefix-p-mid-formula ()
  "Point mid-formula returns nil."
  (with-calc-stack ("x + 1")
    (calc-cursor-stack-index 1)
    (end-of-line)
    (backward-char 2)
    (should-not (my/calc-point-is-in-line-prefix-p))))

(ert-deftest test-my/calc-point-is-in-line-prefix-p-at-eol ()
  "Point at end of line returns nil."
  (with-calc-stack ("x + 1")
    (calc-cursor-stack-index 1)
    (end-of-line)
    (should-not (my/calc-point-is-in-line-prefix-p))))

(provide 'my-calc-point-is-in-line-prefix-tests)
