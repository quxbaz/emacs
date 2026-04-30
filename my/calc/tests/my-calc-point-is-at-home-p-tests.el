;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-point-is-at-home-p

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'calc-yank)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))


;;; Helper

(defmacro with-calc-stack (exprs &rest body)
  "Set up a calc buffer with EXPRS pushed onto the stack, then run BODY.
EXPRS is a list of expression strings."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     ,@(mapcar (lambda (e) `(calc-push (math-read-expr ,e))) exprs)
     ,@body))


;;; Empty stack

(ert-deftest test-my-calc-point-is-at-home-p-empty-stack ()
  "Empty stack: point is always at home."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (goto-char (point-max))
    (should (my/calc-point-is-at-home-p))))


;;; At the . line

(ert-deftest test-my-calc-point-is-at-home-p-dot-line ()
  "Point on the . line returns t."
  (with-calc-stack ("5")
    (goto-char (point-max))
    (forward-line -1)
    (should (my/calc-point-is-at-home-p))))

(ert-deftest test-my-calc-point-is-at-home-p-below-dot-line ()
  "Point below the . line (point-max) returns t."
  (with-calc-stack ("5")
    (goto-char (point-max))
    (should (my/calc-point-is-at-home-p))))


;;; On a stack entry

(ert-deftest test-my-calc-point-is-at-home-p-at-entry-1 ()
  "Point on stack entry 1 returns nil."
  (with-calc-stack ("5")
    (calc-cursor-stack-index 1)
    (should-not (my/calc-point-is-at-home-p))))

(ert-deftest test-my-calc-point-is-at-home-p-at-entry-2 ()
  "Point on stack entry 2 (bottom of two) returns nil."
  (with-calc-stack ("5" "x + 1")
    (calc-cursor-stack-index 2)
    (should-not (my/calc-point-is-at-home-p))))

(ert-deftest test-my-calc-point-is-at-home-p-at-top-entry ()
  "Point on the top entry (entry 1) of a multi-entry stack returns nil."
  (with-calc-stack ("5" "x + 1")
    (calc-cursor-stack-index 1)
    (should-not (my/calc-point-is-at-home-p))))
