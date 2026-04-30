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
