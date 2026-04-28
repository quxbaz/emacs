;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-plus

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))


;;; Helper
;; Use car/nth directly — calc-top-n re-normalizes and changes the equation form.

(defmacro calc-plus-test (a b expected)
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (setq calc-symbolic-mode t calc-prefer-frac t)
     (calc-push (math-read-expr ,a))
     (calc-push (math-read-expr ,b))
     (my/calc-plus nil)
     (should (equal (math-format-value (car (nth 1 calc-stack)))
                    (math-format-value (math-read-expr ,expected))))))


;;; Equation + equation

(ert-deftest test-calc-plus-equations-add ()
  "x^2+y^2=17 plus x^2-y^2=8 gives 2*x^2=25."
  (calc-plus-test "x^2 + y^2 = 17" "x^2 - y^2 = 8" "2*x^2 = 25"))

(ert-deftest test-calc-plus-equations-linear ()
  "2x+y=5 plus x-y=1 gives 3x=6."
  (calc-plus-test "2*x + y = 5" "x - y = 1" "3*x = 6"))

(ert-deftest test-calc-plus-equations-rhs-adds ()
  "RHS values are summed correctly."
  (calc-plus-test "x = 3" "y = 4" "x + y = 7"))


;;; Equation + equation — more cases

(ert-deftest test-calc-plus-equations-symbolic-rhs ()
  "Symbolic RHS: a=x+1 plus b=x-1 gives a+b=2*x."
  (calc-plus-test "a = x + 1" "b = x - 1" "a + b = 2*x"))

(ert-deftest test-calc-plus-equations-self ()
  "Equation added to itself: x=5 plus x=5 gives 2*x=10."
  (calc-plus-test "x = 5" "x = 5" "2*x = 10"))

(ert-deftest test-calc-plus-equations-rhs-cancels ()
  "RHS cancels to zero: x+y=3 plus x-y=-3 gives 2*x=0."
  (calc-plus-test "x + y = 3" "x - y = -3" "2*x = 0"))

(ert-deftest test-calc-plus-equations-stack-depth ()
  "With 3 items on stack, only top 2 are consumed; 2 items remain."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (setq calc-symbolic-mode t calc-prefer-frac t)
    (calc-push (math-read-expr "z = 99"))
    (calc-push (math-read-expr "x = 3"))
    (calc-push (math-read-expr "y = 4"))
    (my/calc-plus nil)
    (should (= (calc-stack-size) 2))
    (should (equal (math-format-value (car (nth 1 calc-stack)))
                   (math-format-value (math-read-expr "x + y = 7"))))))


;;; Fallback to calc-plus for non-equations

(ert-deftest test-calc-plus-numbers ()
  "Plain numbers fall through to calc-plus."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 3)
    (calc-push 4)
    (my/calc-plus nil)
    (should (equal (car (nth 1 calc-stack)) 7))))

(ert-deftest test-calc-plus-mixed ()
  "Number + equation falls through to calc-plus (leaves unevaluated)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 5)
    (calc-push (math-read-expr "x = 3"))
    (my/calc-plus nil)
    ;; Should not error; stack has 2 items (not collapsed to equation form)
    (should (= (calc-stack-size) 1))))
