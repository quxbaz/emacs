;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-edit

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/util.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/edit.el" user-emacs-directory))


;;; Point preservation on open

(ert-deftest test-my-calc-edit-point-not-moved-on-open ()
  "Opening the edit buffer via my/calc-edit does not move point in the calc buffer.
Regression: calc-align-stack-window inside calc-edit would scroll the calc
window and leave point at the wrong position."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (dotimes (_ 10)
      (calc-push (math-read-expr "x + 1")))
    (calc-cursor-stack-index 5)
    (let ((saved (point))
          (calc-buf (current-buffer)))
      (my/calc-edit 1)
      (with-current-buffer calc-buf
        (should (= (point) saved))))))


;;; No simplification on finish

(defmacro with-calc-edit-entry (initial-expr edit-text &rest body)
  "Set up a calc buffer with INITIAL-EXPR, open edit buffer, replace with EDIT-TEXT, run BODY."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,initial-expr))
     (my/calc-edit 1)
     (goto-char (point-min)) (forward-line 2) (delete-region (point) (point-max))
     (insert ,edit-text)
     ,@body))

(ert-deftest test-my-calc-edit-finish-no-simplification-symbolic ()
  "my/calc-edit-finish preserves x+5+3 as-is without collapsing constants."
  (with-calc-edit-entry "1" "x+5+3"
    (my/calc-edit-finish)
    (should (string= (math-format-nice-expr (calc-top 1) 80) "x + 5 + 3"))))

(ert-deftest test-my-calc-edit-finish-no-simplification-numeric ()
  "my/calc-edit-finish preserves 5+3 without evaluating to 8."
  (with-calc-edit-entry "1" "5+3"
    (my/calc-edit-finish)
    (should (string= (math-format-nice-expr (calc-top 1) 80) "5 + 3"))))

(ert-deftest test-my-calc-edit-finish-simplify-mode-restored ()
  "my/calc-edit-finish restores calc-simplify-mode in the calc buffer after finishing."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let ((mode-before calc-simplify-mode))
      (calc-push (math-read-expr "1"))
      (my/calc-edit 1)
      (goto-char (point-min)) (forward-line 2) (delete-region (point) (point-max))
      (insert "1")
      (my/calc-edit-finish)
      (should (eq calc-simplify-mode mode-before)))))


;;; Post-finish alignment

(ert-deftest test-my-calc-edit-finish-calls-align ()
  "my/calc-edit-finish calls calc-align-stack-window after my/calc-edit."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "5"))
    (calc-cursor-stack-index 1)
    (my/calc-edit 1)
    (let (align-called)
      (cl-letf (((symbol-function 'calc-align-stack-window)
                 (lambda () (setq align-called t))))
        (my/calc-edit-finish))
      (should align-called))))
