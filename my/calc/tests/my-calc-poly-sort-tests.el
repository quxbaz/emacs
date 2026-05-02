;; -*- lexical-binding: t; -*-
;;
;; Tests for polynomial term sorting (my/calc-poly-sort-sum advice on math-normalize)

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))

;;; Sorting active (normal mode)

(ert-deftest test-my/calc-poly-sort-reorders-sum ()
  "(2-x) in normal mode is sorted to (-x+2): variable term before constant."
  (with-temp-buffer
    (calc-mode)
    (let ((result (calc-normalize (math-read-expr "2-x"))))
      (should (eq (car-safe result) '+)))))

(ert-deftest test-my/calc-poly-sort-reorders-base-in-power ()
  "(2-x)^2 pushed normally: the base (2-x) is reordered to (-x+2)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-wrapper
     (calc-enter-result 0 "t" (list (math-read-expr "(2-x)^2"))))
    (let* ((result (car (nth 1 calc-stack)))
           (base   (nth 1 result)))           ; base of (^ base 2)
      (should (eq (car-safe result) '^))
      (should (eq (car-safe base) '+)))))     ; (-x+2) = (+ ...)

;;; Sorting suppressed (no-simplify mode)

(ert-deftest test-my/calc-poly-sort-skipped-in-no-simplify ()
  "(2-x) with calc-simplify-mode='none stays as (2-x), not reordered."
  (with-temp-buffer
    (calc-mode)
    (let* ((calc-simplify-mode 'none)
           (result (calc-normalize (math-read-expr "2-x"))))
      (should (eq (car-safe result) '-)))))

(ert-deftest test-my/calc-poly-sort-skipped-in-power-with-no-simplify ()
  "(2-x)^2 pushed under my/calc-without-simplification: base stays as (2-x)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (my/calc-without-simplification
      (calc-wrapper
       (calc-enter-result 0 "t" (list (math-read-expr "(2-x)^2")))))
    (let* ((result (car (nth 1 calc-stack)))
           (base   (nth 1 result)))
      (should (eq (car-safe result) '^))
      (should (eq (car-safe base) '-)))))    ; (2-x) = (- ...)

(provide 'my-calc-poly-sort-tests)
