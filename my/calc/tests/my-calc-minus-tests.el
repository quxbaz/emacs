;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-minus

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))


;;; Helper
;; Use car/nth directly — calc-top-n re-normalizes and changes the equation form.

(defmacro calc-minus-test (a b expected)
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (setq calc-symbolic-mode t calc-prefer-frac t)
     (calc-push (math-read-expr ,a))
     (calc-push (math-read-expr ,b))
     (my/calc-minus nil)
     (should (equal (math-format-value (car (nth 1 calc-stack)))
                    (math-format-value (math-read-expr ,expected))))))


;;; Equation - equation

(ert-deftest test-calc-minus-equations-cancel ()
  "x^2+y^2=17 minus x^2+y^2=17 gives 1 (Calc normalizes 0=0 to true)."
  (calc-minus-test "x^2 + y^2 = 17" "x^2 + y^2 = 17" "1"))

(ert-deftest test-calc-minus-equations-isolate ()
  "x^2+y^2=17 minus x^2-y^2=8 gives 2*y^2=9."
  (calc-minus-test "x^2 + y^2 = 17" "x^2 - y^2 = 8" "2*y^2 = 9"))

(ert-deftest test-calc-minus-equations-linear ()
  "2x+y=5 minus x+y=3 gives x=2."
  (calc-minus-test "2*x + y = 5" "x + y = 3" "x = 2"))

(ert-deftest test-calc-minus-equations-rhs-subtracts ()
  "RHS values are differenced correctly."
  (calc-minus-test "x = 7" "y = 3" "x - y = 4"))

(ert-deftest test-calc-minus-equations-symbolic-rhs ()
  "Symbolic RHS: a=x+1 minus b=x-1 gives a-b=2."
  (calc-minus-test "a = x + 1" "b = x - 1" "a - b = 2"))

(ert-deftest test-calc-minus-equations-stack-depth ()
  "With 3 items on stack, only top 2 are consumed; 2 items remain."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (setq calc-symbolic-mode t calc-prefer-frac t)
    (calc-push (math-read-expr "z = 99"))
    (calc-push (math-read-expr "x^2 + y^2 = 17"))
    (calc-push (math-read-expr "x^2 - y^2 = 8"))
    (my/calc-minus nil)
    (should (= (calc-stack-size) 2))
    (should (equal (math-format-value (car (nth 1 calc-stack)))
                   (math-format-value (math-read-expr "2*y^2 = 9"))))))


;;; Fallback to calc-minus for non-equations

(ert-deftest test-calc-minus-numbers ()
  "Plain numbers fall through to calc-minus."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 7)
    (calc-push 3)
    (my/calc-minus nil)
    (should (equal (car (nth 1 calc-stack)) 4))))

(ert-deftest test-calc-minus-mixed ()
  "Number - equation falls through to calc-minus (leaves unevaluated)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 5)
    (calc-push (math-read-expr "x = 3"))
    (my/calc-minus nil)
    (should (= (calc-stack-size) 1))))
