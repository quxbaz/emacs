;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-evaluate and my/calc--identify-sqrt

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))

;;; my/calc--identify-sqrt

(ert-deftest test-my/calc--identify-sqrt-sqrt6 ()
  "2.44948974278 is identified as (calcFunc-sqrt 6)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should (equal (my/calc--identify-sqrt (math-read-expr "2.44948974278"))
                   '(calcFunc-sqrt 6)))))

(ert-deftest test-my/calc--identify-sqrt-sqrt2 ()
  "1.41421356237 is identified as (calcFunc-sqrt 2)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should (equal (my/calc--identify-sqrt (math-read-expr "1.41421356237"))
                   '(calcFunc-sqrt 2)))))

(ert-deftest test-my/calc--identify-sqrt-integer ()
  "Integer 3 is identified as (calcFunc-sqrt 9)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should (equal (my/calc--identify-sqrt 3)
                   '(calcFunc-sqrt 9)))))

(ert-deftest test-my/calc--identify-sqrt-no-match ()
  "A number with no nearby integer square root signals an error."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (should-error (my/calc--identify-sqrt (math-read-expr "1.23456789")))))

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

(ert-deftest test-my/calc-evaluate-inverse-sqrt2 ()
  "I k k on 1.41421356237 produces the symbolic form (calcFunc-sqrt 2)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "1.41421356237"))
    (let ((calc-inverse-flag t))
      (my/calc-evaluate 1))
    (should (equal (car (nth 1 calc-stack)) '(calcFunc-sqrt 2)))))

(provide 'my-calc-evaluate-tests)
