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


;;; Point preservation

(ert-deftest test-my-calc-edit-dwim-point-not-moved-on-open ()
  "Opening the edit buffer does not move point in the calc buffer.
Regression: calc-align-stack-window was called during edit buffer open,
scrolling the calc window and leaving point at the wrong position."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (dotimes (_ 10)
      (calc-push (math-read-expr "x + 1")))
    (calc-cursor-stack-index 5)
    (let ((saved (point))
          (calc-buf (current-buffer)))
      (my/calc-edit-dwim)
      (with-current-buffer calc-buf
        (should (= (point) saved))))))

(ert-deftest test-my-calc-edit-dwim-point-restored ()
  "Point is restored to its pre-edit position after my/calc-edit-finish."
  (with-calc-edit-dwim-setup "5"
    (let ((saved (point)))
      (my/calc-edit-dwim)
      (my/calc-edit-finish)
      (should (= (point) saved)))))


;;; Contextual editing

(ert-deftest test-my-calc-edit-dwim-subexpr-at-operator ()
  "Cursor on the * operator in x+y*z opens edit buffer with just y*z."
  (with-calc-edit-dwim-setup "x + y*z"
    (forward-char 9)                    ; col 9: between y and z (the * position)
    (my/calc-edit-dwim)
    (should (string= (string-trim
                      (buffer-substring-no-properties calc-edit-top (point-max)))
                     "y * z"))))

(ert-deftest test-my-calc-edit-dwim-eol-whole-formula ()
  "Cursor at EOL (past the formula) opens edit buffer with the whole formula."
  (with-calc-edit-dwim-setup "x + y*z"
    (end-of-line)
    (my/calc-edit-dwim)
    (should (string= (string-trim
                      (buffer-substring-no-properties calc-edit-top (point-max)))
                     "x + y * z"))))

(ert-deftest test-my-calc-edit-dwim-home-new-entry-path ()
  "At the . line, my/calc-edit-dwim takes the new-entry path, not the selection path."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "5"))
    (goto-char (point-max))
    (forward-line -1)                   ; . line
    (let (selection-called)
      (cl-letf (((symbol-function 'my/calc-edit-selection)
                 (lambda () (setq selection-called t)))
                ((symbol-function 'execute-kbd-macro) #'ignore))
        (my/calc-edit-dwim))
      (should-not selection-called))))


;;; Post-finish alignment

(ert-deftest test-my-calc-edit-finish-new-entry-calls-align ()
  "my/calc-edit-finish calls calc-align-stack-window when my/calc-edit-new-entry is t."
  (with-calc-edit-dwim-setup "5"
    (my/calc-edit-dwim)                  ; opens edit buffer (stack entry, new-entry=nil)
    (let ((my/calc-edit-new-entry t)     ; override: simulate new-entry path
          align-called)
      (cl-letf (((symbol-function 'calc-align-stack-window)
                 (lambda () (setq align-called t))))
        (my/calc-edit-finish))
      (should align-called))))

(ert-deftest test-my-calc-edit-finish-existing-entry-does-not-align ()
  "my/calc-edit-finish does not call calc-align-stack-window for existing entries."
  (with-calc-edit-dwim-setup "5"
    (my/calc-edit-dwim)                  ; my/calc-edit-new-entry=nil (on stack entry)
    (let (align-called)
      (cl-letf (((symbol-function 'calc-align-stack-window)
                 (lambda () (setq align-called t))))
        (my/calc-edit-finish))
      (should-not align-called))))

(ert-deftest test-my-calc-edit-finish-existing-entry-restores-point ()
  "my/calc-edit-finish restores point for existing entries, not new ones."
  (with-calc-edit-dwim-setup "5"
    (let ((saved (point)))
      (my/calc-edit-dwim)
      (let ((my/calc-edit-new-entry nil))
        (my/calc-edit-finish))
      (should (= (point) saved)))))
