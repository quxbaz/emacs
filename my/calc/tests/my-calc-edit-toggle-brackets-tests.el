;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-edit-toggle-brackets

(require 'ert)

(load-file (expand-file-name "my/calc/edit.el" user-emacs-directory))


;;; Helper

(defmacro with-toggle-buffer (text col &rest body)
  "Run BODY in a temp buffer with TEXT, point at column COL (0-indexed)."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (+ (point-min) ,col))
     ,@body
     (buffer-string)))


;;; ( -> [

(ert-deftest test-toggle-brackets-open-paren-to-bracket ()
  "( at point becomes [."
  (should (equal (with-toggle-buffer "(x+y)" 0
                   (my/calc-edit-toggle-brackets))
                 "[x+y)")))

;;; ) -> ]

(ert-deftest test-toggle-brackets-close-paren-to-bracket ()
  ") at point becomes ]."
  (should (equal (with-toggle-buffer "(x+y)" 4
                   (my/calc-edit-toggle-brackets))
                 "(x+y]")))

;;; [ -> (

(ert-deftest test-toggle-brackets-open-bracket-to-paren ()
  "[ at point becomes (."
  (should (equal (with-toggle-buffer "[x+y]" 0
                   (my/calc-edit-toggle-brackets))
                 "(x+y]")))

;;; ] -> )

(ert-deftest test-toggle-brackets-close-bracket-to-paren ()
  "] at point becomes )."
  (should (equal (with-toggle-buffer "[x+y]" 4
                   (my/calc-edit-toggle-brackets))
                 "[x+y)")))

;;; Point stays on toggled char

(ert-deftest test-toggle-brackets-point-stays ()
  "Point remains on the toggled delimiter after the command."
  (with-temp-buffer
    (insert "(x+y)")
    (goto-char (point-min))
    (my/calc-edit-toggle-brackets)
    (should (= (point) (point-min)))))

;;; No-op on non-delimiter

(ert-deftest test-toggle-brackets-noop-on-other-char ()
  "No change when point is not on a delimiter."
  (should (equal (with-toggle-buffer "(x+y)" 1
                   (my/calc-edit-toggle-brackets))
                 "(x+y)")))

;;; Repeated toggle cycles back

(ert-deftest test-toggle-brackets-round-trip ()
  "Two toggles restore the original delimiter."
  (should (equal (with-toggle-buffer "(x+y)" 0
                   (my/calc-edit-toggle-brackets)
                   (my/calc-edit-toggle-brackets))
                 "(x+y)")))
