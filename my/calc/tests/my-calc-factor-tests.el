;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-factor

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

(defun my-calc-factor-test--expanded-equal (a b)
  "Return t if expressions A and B expand to the same polynomial."
  (math-zerop (math-simplify
               (math-normalize
                (list '- (calcFunc-expand a) (calcFunc-expand b))))))

;;; Basic stack tests

(ert-deftest test-my/calc-factor-difference-of-squares ()
  "x^2 - 1 factors to (x+1)(x-1)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x^2 - 1"))
    (my/calc-factor)
    (let ((result (car (nth 1 calc-stack))))
      (should (my-calc-factor-test--expanded-equal result (math-read-expr "x^2 - 1"))))))

;;; Line-targeting test: non-top entry

(ert-deftest test-my/calc-factor-level2 ()
  "Factor x^2-1 at level 2 (non-top entry), with 1 at level 1, cursor at eol."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x^2 - 1"))
    (calc-push 1)
    ;; Stack: 2: x^2-1, 1: 1
    (calc-cursor-stack-index 2)
    (end-of-line)
    (my/calc-factor)
    (let ((result (car (nth 2 calc-stack)))
          (level1 (car (nth 1 calc-stack))))
      ;; Level 1 (the 1) should be untouched
      (should (equal level1 1))
      ;; Level 2 should be the factored form, expanding back to x^2-1
      (should (my-calc-factor-test--expanded-equal result (math-read-expr "x^2 - 1"))))))

;;; Verify no (cplx ...) atom corruption in result

(ert-deftest test-my/calc-factor-no-cplx-atoms ()
  "Factoring at level 2 must not corrupt atoms with (cplx ...) wrappers."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x^2 - 1"))
    (calc-push 1)
    (calc-cursor-stack-index 2)
    (end-of-line)
    (my/calc-factor)
    (let ((result (car (nth 2 calc-stack))))
      (should-not (member 'cplx (flatten-tree result))))))

(provide 'my-calc-factor-tests)
