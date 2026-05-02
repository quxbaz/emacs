;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-cath

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))


;;; Helper

(defmacro calc-cath-test (hyp leg expected)
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (setq calc-symbolic-mode t calc-prefer-frac t)
     (calc-push (math-read-expr ,hyp))
     (calc-push (math-read-expr ,leg))
     (my/calc-cath)
     (should (equal (math-format-value (car (nth 1 calc-stack)))
                    (math-format-value (math-read-expr ,expected))))))


;;; Pythagorean triples

(ert-deftest test-my/calc-cath-3-4-5 ()
  "cath(5, 3) = 4."
  (calc-cath-test "5" "3" "4"))

(ert-deftest test-my/calc-cath-5-12-13 ()
  "cath(13, 5) = 12."
  (calc-cath-test "13" "5" "12"))

(ert-deftest test-my/calc-cath-8-15-17 ()
  "cath(17, 8) = 15."
  (calc-cath-test "17" "8" "15"))


;;; Symbolic / irrational

(ert-deftest test-my/calc-cath-sqrt2-1-1 ()
  "cath(sqrt(2), 1) = 1."
  (calc-cath-test "sqrt(2)" "1" "1"))

(ert-deftest test-my/calc-cath-2-sqrt3 ()
  "cath(2, 1) = sqrt(3)."
  (calc-cath-test "2" "1" "sqrt(3)"))


;;; Stack behaviour

(ert-deftest test-my/calc-cath-always-pops-2 ()
  "Always pops exactly 2 items regardless of stack depth."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "99"))
    (calc-push (math-read-expr "5"))
    (calc-push (math-read-expr "3"))
    (my/calc-cath)
    (should (= (calc-stack-size) 2))
    (should (equal (math-format-value (car (nth 1 calc-stack)))
                   (math-format-value (math-read-expr "4"))))))

(provide 'my-calc-cath-tests)
