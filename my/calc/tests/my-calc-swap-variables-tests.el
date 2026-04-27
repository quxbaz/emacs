;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-swap-variables

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))


;;; Helper

(defmacro swap-test (input swap-input expected)
  "Push INPUT, call swap with SWAP-INPUT, compare result to EXPECTED."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,input))
     (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ,swap-input)))
       (my/calc-swap-variables))
     (should (equal (math-normalize (car (nth 1 calc-stack)))
                    (math-normalize (math-read-expr ,expected))))))


;;; Basic swap

(ert-deftest test-swap-variables-basic ()
  "2y = x+2 -> 2x = y+2."
  (swap-test "2*y = x + 2" "x y" "2*x = y + 2"))

(ert-deftest test-swap-variables-symmetric ()
  "Swapping in the other order gives the same result."
  (swap-test "2*y = x + 2" "y x" "2*x = y + 2"))

(ert-deftest test-swap-variables-expression ()
  "Works on bare expressions, not just equations."
  (swap-test "x^2 + y" "x y" "y^2 + x"))

(ert-deftest test-swap-variables-with-param ()
  "Variables not named in the swap are left untouched."
  (swap-test "a*x + b*y" "x y" "a*y + b*x"))


;;; Input format variants

(ert-deftest test-swap-variables-comma ()
  "a,b format."
  (swap-test "x + y" "x,y" "y + x"))

(ert-deftest test-swap-variables-brackets ()
  "[a,b] format."
  (swap-test "x + y" "[x,y]" "y + x"))

(ert-deftest test-swap-variables-brackets-space ()
  "[a b] format."
  (swap-test "x + y" "[x y]" "y + x"))


;;; Default variable detection

(ert-deftest test-swap-variables-default ()
  "Empty input defaults to the first two auto-detected variables."
  (swap-test "x^2 + y" "" "y^2 + x"))
