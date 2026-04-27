;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-unique-groups

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))


;;; Helper

(defmacro unique-groups-test (vec n expected)
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,vec))
     (calc-push ,n)
     (my/calc-unique-groups)
     (should (equal (math-normalize (calc-top-n 1))
                    (math-normalize (math-read-expr ,expected))))))


;;; Basic cases

(ert-deftest test-unique-groups-pairs ()
  "4 elements, n=2: 6 pairs."
  (unique-groups-test "[a,b,c,d]" 2 "[[a,b],[a,c],[a,d],[b,c],[b,d],[c,d]]"))

(ert-deftest test-unique-groups-triples ()
  "4 elements, n=3: 4 triples."
  (unique-groups-test "[a,b,c,d]" 3 "[[a,b,c],[a,b,d],[a,c,d],[b,c,d]]"))

(ert-deftest test-unique-groups-all ()
  "n equals vector length: one group containing all elements."
  (unique-groups-test "[a,b,c]" 3 "[[a,b,c]]"))

(ert-deftest test-unique-groups-singles ()
  "n=1: each element wrapped in its own group."
  (unique-groups-test "[a,b,c]" 1 "[[a],[b],[c]]"))

(ert-deftest test-unique-groups-n-exceeds-length ()
  "n greater than vector length: empty result."
  (unique-groups-test "[a,b]" 3 "[]"))

(ert-deftest test-unique-groups-two-elements ()
  "2 elements, n=2: one pair."
  (unique-groups-test "[a,b]" 2 "[[a,b]]"))


;;; Default n=2

(ert-deftest test-unique-groups-default-n ()
  "Vector alone on stack defaults to n=2."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "[a,b,c]"))
    (my/calc-unique-groups)
    (should (equal (math-normalize (calc-top-n 1))
                   (math-normalize (math-read-expr "[[a,b],[a,c],[b,c]]"))))))
