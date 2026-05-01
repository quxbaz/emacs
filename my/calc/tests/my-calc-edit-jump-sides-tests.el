;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-edit-jump-sides

(require 'ert)

(load-file (expand-file-name "my/calc/edit.el" user-emacs-directory))


;;; Helper

(defmacro with-tab-buffer (init-text &rest body)
  "Run BODY in a temp buffer pre-populated with INIT-TEXT.
Point starts at end of buffer. Local tab vars are reset before each test."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,init-text)
     (setq my/calc-edit-tab-lhs nil
           my/calc-edit-tab-rhs nil)
     ,@body))


;;; Inserting '='

(ert-deftest test-calc-edit-tab-inserts-equals ()
  "TAB on a line with no '=' appends ' = ' at end of line."
  (with-tab-buffer "x+1"
    (my/calc-edit-jump-sides)
    (should (equal (buffer-string) "x+1 = "))))

(ert-deftest test-calc-edit-tab-strips-trailing-space ()
  "TAB strips trailing whitespace before inserting ' = '."
  (with-tab-buffer "x+1   "
    (my/calc-edit-jump-sides)
    (should (equal (buffer-string) "x+1 = "))))

(ert-deftest test-calc-edit-tab-saves-lhs-position ()
  "After inserting '=', lhs origin is the pre-TAB point."
  (with-tab-buffer "x+1"
    (goto-char 2)                         ; after 'x'
    (my/calc-edit-jump-sides)
    (should (= my/calc-edit-tab-lhs 2))))


;;; LHS → RHS

(ert-deftest test-calc-edit-tab-lhs-to-rhs-computed ()
  "TAB on LHS with no saved RHS jumps to first non-WS after '='."
  (with-tab-buffer "x+1 = y+2"
    (goto-char 1)
    (my/calc-edit-jump-sides)
    (should (= (point) 7))))              ; position of 'y'

(ert-deftest test-calc-edit-tab-lhs-to-rhs-saved ()
  "TAB on LHS with a saved RHS position jumps to it."
  (with-tab-buffer "x+1 = y+2"
    (goto-char 1)
    (setq my/calc-edit-tab-rhs 9)        ; saved somewhere mid-RHS
    (my/calc-edit-jump-sides)
    (should (= (point) 9))))

(ert-deftest test-calc-edit-tab-lhs-saves-position ()
  "Jumping LHS→RHS saves the LHS position."
  (with-tab-buffer "x+1 = y+2"
    (goto-char 3)
    (my/calc-edit-jump-sides)
    (should (= my/calc-edit-tab-lhs 3))))


;;; RHS → LHS

(ert-deftest test-calc-edit-tab-rhs-to-lhs-saved ()
  "TAB on RHS with a saved LHS position jumps to it."
  (with-tab-buffer "x+1 = y+2"
    (goto-char 9)                         ; on RHS
    (setq my/calc-edit-tab-lhs 3)
    (my/calc-edit-jump-sides)
    (should (= (point) 3))))

(ert-deftest test-calc-edit-tab-rhs-to-lhs-bol ()
  "TAB on RHS with no saved LHS jumps to beginning of line."
  (with-tab-buffer "x+1 = y+2"
    (goto-char 9)
    (my/calc-edit-jump-sides)
    (should (= (point) 1))))

(ert-deftest test-calc-edit-tab-rhs-saves-position ()
  "Jumping RHS→LHS saves the RHS position."
  (with-tab-buffer "x+1 = y+2"
    (goto-char 9)
    (my/calc-edit-jump-sides)
    (should (= my/calc-edit-tab-rhs 9))))


;;; Full toggle cycle

(ert-deftest test-calc-edit-tab-toggle-cycle ()
  "TAB toggles back and forth between saved positions."
  (with-tab-buffer "x+1 = y+2"
    (goto-char 3)                         ; LHS: after '+'
    (my/calc-edit-jump-sides)                    ; → RHS start ('y')
    (should (= (point) 7))
    (my/calc-edit-jump-sides)                    ; → back to LHS (3)
    (should (= (point) 3))
    (my/calc-edit-jump-sides)                    ; → RHS (saved: 7)
    (should (= (point) 7))))

(ert-deftest test-calc-edit-tab-insert-then-toggle ()
  "Insert '=' via TAB then toggle back to the origin position."
  (with-tab-buffer "x+1"
    (goto-char 2)                         ; after 'x'
    (my/calc-edit-jump-sides)                    ; inserts ' = ', cursor after it
    (should (string-match-p "=" (buffer-string)))
    (my/calc-edit-jump-sides)                    ; RHS→LHS: back to pos 2
    (should (= (point) 2))))
