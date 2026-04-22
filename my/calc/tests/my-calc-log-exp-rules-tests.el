;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-log-exp-rules
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/rewrite.el" user-emacs-directory))

(defmacro my-calc-log-exp-rules-test (input expected)
  "Push INPUT, run my/calc-log-exp-rules, compare result to EXPECTED."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,input))
     (my/calc-log-exp-rules)
     (should (equal (car (nth 1 calc-stack))
                    (math-read-expr ,expected)))))

;;; b^log(x, b) = x  (general)

(ert-deftest test-my/calc-log-exp-rules-b-pow-log-symbolic ()
  "b^log(x, b) -> x."
  (my-calc-log-exp-rules-test "b^log(x, b)" "x"))

;;; b^(-log(x, b)) = 1/x  (general)

(ert-deftest test-my/calc-log-exp-rules-b-pow-neg-log-symbolic ()
  "b^(-log(x, b)) -> 1/x."
  (my-calc-log-exp-rules-test "b^(-log(x, b))" "1/x"))

;;; e^ln(x) = x

(ert-deftest test-my/calc-log-exp-rules-e-pow-ln ()
  "e^ln(x) -> x."
  (my-calc-log-exp-rules-test "e^ln(x)" "x"))

;;; e^(-ln(x)) = 1/x

(ert-deftest test-my/calc-log-exp-rules-e-pow-neg-ln ()
  "e^(-ln(x)) -> 1/x."
  (my-calc-log-exp-rules-test "e^(-ln(x))" "1/x"))

;;; 10^log10(x) = x  (base-10; calc normalizes log(x,10) to log10(x))

(ert-deftest test-my/calc-log-exp-rules-10-pow-log10 ()
  "10^log10(x) -> x."
  (my-calc-log-exp-rules-test "10^log10(x)" "x"))

;;; 10^(-log10(x)) = 1/x  (base-10 negative exponent)

(ert-deftest test-my/calc-log-exp-rules-10-pow-neg-log10 ()
  "10^(-log10(x)) -> 1/x."
  (my-calc-log-exp-rules-test "10^(-log10(x))" "1/x"))

;;; ln(x^p) = p * ln(x)

(ert-deftest test-my/calc-log-exp-rules-ln-power-numeric-exp ()
  "ln(x^3) -> 3*ln(x)."
  (my-calc-log-exp-rules-test "ln(x^3)" "3*ln(x)"))

(ert-deftest test-my/calc-log-exp-rules-ln-power-symbolic-exp ()
  "ln(x^p) -> p*ln(x)."
  (my-calc-log-exp-rules-test "ln(x^p)" "p*ln(x)"))

;;; log(x^p, b) = p * log(x, b)

(ert-deftest test-my/calc-log-exp-rules-log-power-symbolic-exp ()
  "log(x^p, b) -> p*log(x, b)."
  (my-calc-log-exp-rules-test "log(x^p, b)" "p*log(x, b)"))

;;; log10(x^p) = p * log10(x)  (base-10; calc normalizes log(x^p,10) to log10(x^p))

(ert-deftest test-my/calc-log-exp-rules-log10-power-numeric-exp ()
  "log10(x^3) -> 3*log10(x)."
  (my-calc-log-exp-rules-test "log10(x^3)" "3*log10(x)"))

(ert-deftest test-my/calc-log-exp-rules-log10-power-symbolic-exp ()
  "log10(x^p) -> p*log10(x)."
  (my-calc-log-exp-rules-test "log10(x^p)" "p*log10(x)"))

(provide 'my-calc-log-exp-rules-tests)
