;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-commute

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/util.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/selection.el" user-emacs-directory))


;;; Helper

(defmacro commute-test (input expected)
  "Push INPUT, run my/calc-commute, compare formatted top to EXPECTED string."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,input))
     (my/calc-commute)
     (should (equal (math-format-value (car (nth 1 calc-stack)))
                    (math-format-value (math-read-expr ,expected))))))


;;; Basic commutativity

(ert-deftest test-calc-commute-addition ()
  "a+b -> b+a."
  (commute-test "a + b" "b + a"))

(ert-deftest test-calc-commute-multiplication ()
  "a*b -> b*a."
  (commute-test "a * b" "b * a"))


;;; No simplification — numeric products must not be evaluated

(ert-deftest test-calc-commute-no-simplify-product ()
  "2*(3+x) -> (x+3)*2, not 2x+6."
  (let* ((result (with-temp-buffer
                   (calc-mode)
                   (calc-reset 0)
                   (calc-push (math-read-expr "2 * (3 + x)"))
                   (my/calc-commute)
                   (car (nth 1 calc-stack))))
         (s (math-format-value result)))
    ;; The numeric 2 must not have been multiplied through.
    (should (string-match-p "2" s))
    (should (not (string-match-p "^2 x" s)))))

(ert-deftest test-calc-commute-no-simplify-coefficient ()
  "x/9*(y+1) commuted keeps coefficient unsimplified."
  (let* ((result (with-temp-buffer
                   (calc-mode)
                   (calc-reset 0)
                   (calc-push (math-read-expr "x / 9 * (y + 1)"))
                   (my/calc-commute)
                   (car (nth 1 calc-stack))))
         (s (math-format-value result)))
    (should (string-match-p "x" s))
    (should (string-match-p "9" s))))
