;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-evaluate and my/calc--identify-expr

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/evaluate.el" user-emacs-directory))

;;; my/calc--identify-expr

(ert-deftest test-my/calc--identify-expr-sqrt6 ()
  "2.44948974278 is identified as (calcFunc-sqrt 6)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should (equal (my/calc--identify-expr (math-read-expr "2.44948974278"))
                   '(calcFunc-sqrt 6)))))

(ert-deftest test-my/calc--identify-expr-sqrt2 ()
  "1.41421356237 is identified as (calcFunc-sqrt 2)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should (equal (my/calc--identify-expr (math-read-expr "1.41421356237"))
                   '(calcFunc-sqrt 2)))))

(ert-deftest test-my/calc--identify-expr-integer ()
  "Integer 3 is identified as 3."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should (equal (my/calc--identify-expr 3) 3))))

(ert-deftest test-my/calc--identify-expr-negative-integer ()
  "Float -3.0 is identified as -3."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should (equal (my/calc--identify-expr (math-read-expr "-3.0")) -3))))

(ert-deftest test-my/calc--identify-expr-fraction ()
  "0.333333333333 is identified as (frac 1 3)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should (equal (my/calc--identify-expr (math-read-expr "0.333333333333"))
                   '(frac 1 3)))))

(ert-deftest test-my/calc--identify-expr-half ()
  "0.5 is identified as (frac 1 2)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should (equal (my/calc--identify-expr (math-read-expr "0.5"))
                   '(frac 1 2)))))

(ert-deftest test-my/calc--identify-expr-p-sqrt ()
  "3.46410161514 ≈ 2·√3 is identified as 2·√3."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let ((result (my/calc--identify-expr (math-read-expr "3.46410161514"))))
      ;; Check it equals 2*sqrt(3) symbolically
      (should (math-zerop
               (math-simplify
                (math-normalize
                 (list '- result (math-read-expr "2*sqrt(3)"))))))))  )

(ert-deftest test-my/calc--identify-expr-cbrt ()
  "1.58740105197 ≈ 4^(1/3) is identified as 4^(1/3)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((result (my/calc--identify-expr (math-read-expr "1.58740105197")))
           (expected (list '^ 4 (list 'frac 1 3)))
           (diff (math-normalize (list '- result expected))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc--identify-expr-pi ()
  "3.14159265359 ≈ π is identified as π."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let ((result (my/calc--identify-expr (math-read-expr "3.14159265359"))))
      (should (equal result '(var pi var-pi))))))

(ert-deftest test-my/calc--identify-expr-pi-half ()
  "1.5707963268 ≈ π/2 is identified as π/2."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((result (my/calc--identify-expr (math-read-expr "1.5707963268")))
           (expected (math-read-expr "pi/2"))
           (diff (math-normalize (list '- result expected))))
      (should (math-zerop (math-simplify diff))))))

(ert-deftest test-my/calc--identify-expr-ln2 ()
  "0.693147180560 ≈ ln(2) is identified as (calcFunc-ln 2)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should (equal (my/calc--identify-expr (math-read-expr "0.693147180560"))
                   '(calcFunc-ln 2)))))

(ert-deftest test-my/calc--identify-expr-no-match ()
  "A number with no nearby simple form signals an error."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should-error (my/calc--identify-expr (math-read-expr "1.23456789012")))))

;;; my/calc-evaluate (normal k k)

(ert-deftest test-my/calc-evaluate-normal ()
  "k k evaluates sqrt(2) to its floating-point value."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let ((calc-symbolic-mode t))
      (calc-push (calc-normalize '(calcFunc-sqrt 2))))
    (my/calc-evaluate 1)
    (let* ((result (car (nth 1 calc-stack)))
           (diff (math-abs (math-sub result (math-read-expr "1.41421356237")))))
      (should (math-lessp diff '(float 1 -6))))))

;;; my/calc-evaluate inverse (I k k)

(ert-deftest test-my/calc-evaluate-inverse-sqrt6 ()
  "I k k on 2.44948974278 produces the symbolic form (calcFunc-sqrt 6)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "2.44948974278"))
    (let ((calc-inverse-flag t))
      (my/calc-evaluate 1))
    (should (equal (car (nth 1 calc-stack)) '(calcFunc-sqrt 6)))))

(ert-deftest test-my/calc-evaluate-inverse-fraction ()
  "I k k on 0.333333333333 produces the symbolic form (frac 1 3)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "0.333333333333"))
    (let ((calc-inverse-flag t))
      (my/calc-evaluate 1))
    (should (equal (car (nth 1 calc-stack)) '(frac 1 3)))))

(ert-deftest test-my/calc-evaluate-inverse-pi ()
  "I k k on 3.14159265359 produces the symbolic form pi."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "3.14159265359"))
    (let ((calc-inverse-flag t))
      (my/calc-evaluate 1))
    (should (equal (car (nth 1 calc-stack)) '(var pi var-pi)))))

(provide 'my-calc-evaluate-tests)
