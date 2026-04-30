;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-edit-dwim

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/util.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/edit.el" user-emacs-directory))


;;; Helper

(defmacro with-calc-edit-dwim-setup (expr &rest body)
  "Calc buffer with EXPR at stack-1, point on that entry, then run BODY."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,expr))
     (calc-cursor-stack-index 1)
     ,@body))


;;; Regression: number entry after calc-enter

(ert-deftest test-my-calc-edit-dwim-number-after-dup-no-error ()
  "Editing a duplicated number entry does not throw 'Original selection has been lost'."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "5"))
    (calc-enter 1)
    (calc-cursor-stack-index 2)
    (my/calc-edit-dwim)
    (my/calc-edit-finish)))


;;; Edit buffer content

(ert-deftest test-my-calc-edit-dwim-edit-buffer-content ()
  "After my/calc-edit-dwim, the edit buffer holds the entry's formula text."
  (with-calc-edit-dwim-setup "42"
    (my/calc-edit-dwim)
    (should (string= (string-trim
                      (buffer-substring-no-properties calc-edit-top (point-max)))
                     "42"))))


;;; Expression entry

(ert-deftest test-my-calc-edit-dwim-expression-no-error ()
  "Editing an expression entry (non-number) completes without error."
  (with-calc-edit-dwim-setup "x + 1"
    (my/calc-edit-dwim)
    (my/calc-edit-finish)))


;;; Point restoration

(ert-deftest test-my-calc-edit-dwim-point-restored ()
  "Point is restored to its pre-edit position after my/calc-edit-finish."
  (with-calc-edit-dwim-setup "5"
    (let ((saved (point)))
      (my/calc-edit-dwim)
      (my/calc-edit-finish)
      (should (= (point) saved)))))
