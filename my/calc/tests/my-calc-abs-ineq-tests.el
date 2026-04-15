;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-abs-ineq
;;

(require 'ert)
(require 'calc)
(require 'cl-lib)
(require 's)

;; Load the code under test
(load-file (expand-file-name "my/calc/rewrite.el" user-emacs-directory))


;;; Helper

(defmacro my/calc-abs-ineq-test (input expected)
  "Push INPUT expression, run my/calc-abs-ineq, and compare result to EXPECTED.
Both INPUT and EXPECTED are algebraic expression strings."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,input))
     (my/calc-abs-ineq)
     (should (equal (car (nth 1 calc-stack))
                    (math-read-expr ,expected)))))


;;; |a| < b  ->  -b < a && a < b

(ert-deftest test-my/calc-abs-ineq-lt-numeric ()
  "Test abs(x) < 5 -> -5 < x && x < 5."
  (my/calc-abs-ineq-test "abs(x) < 5" "-5 < x && x < 5"))

(ert-deftest test-my/calc-abs-ineq-lt-symbolic ()
  "Test abs(x) < b -> -b < x && x < b."
  (my/calc-abs-ineq-test "abs(x) < b" "-b < x && x < b"))

;;; |a| <= b  ->  -b <= a && a <= b

(ert-deftest test-my/calc-abs-ineq-leq-numeric ()
  "Test abs(x) <= 5 -> -5 <= x && x <= 5."
  (my/calc-abs-ineq-test "abs(x) <= 5" "-5 <= x && x <= 5"))

(ert-deftest test-my/calc-abs-ineq-leq-symbolic ()
  "Test abs(x) <= b -> -b <= x && x <= b."
  (my/calc-abs-ineq-test "abs(x) <= b" "-b <= x && x <= b"))


;;; |a| > b  ->  a < -b || a > b

(ert-deftest test-my/calc-abs-ineq-gt-numeric ()
  "Test abs(x) > 5 -> x < -5 || x > 5."
  (my/calc-abs-ineq-test "abs(x) > 5" "x < -5 || x > 5"))

(ert-deftest test-my/calc-abs-ineq-gt-symbolic ()
  "Test abs(x) > b -> x < -b || x > b."
  (my/calc-abs-ineq-test "abs(x) > b" "x < -b || x > b"))

;;; |a| >= b  ->  a <= -b || a >= b

(ert-deftest test-my/calc-abs-ineq-geq-numeric ()
  "Test abs(x) >= 5 -> x <= -5 || x >= 5."
  (my/calc-abs-ineq-test "abs(x) >= 5" "x <= -5 || x >= 5"))

(ert-deftest test-my/calc-abs-ineq-geq-symbolic ()
  "Test abs(x) >= b -> x <= -b || x >= b."
  (my/calc-abs-ineq-test "abs(x) >= b" "x <= -b || x >= b"))


;;; Non-matching expressions are left unchanged

(ert-deftest test-my/calc-abs-ineq-no-match ()
  "Test that non-abs-ineq expressions are left unchanged."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let ((expr (math-read-expr "x + 1")))
      (calc-push expr)
      (my/calc-abs-ineq)
      (should (equal (car (nth 1 calc-stack)) expr)))))

(provide 'my-calc-abs-ineq-tests)
