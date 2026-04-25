;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-coordinate-toggle
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))


;;; Helper

(defmacro my-calc-coordinate-toggle-test (input expected)
  "Push INPUT, run my/calc-coordinate-toggle, compare top-of-stack to EXPECTED."
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (calc-push (math-read-expr ,input))
     (my/calc-coordinate-toggle)
     (should (equal (car (nth 1 calc-stack))
                    (math-read-expr ,expected)))))


;;; Plain vector → named form

(ert-deftest test-my/calc-coordinate-toggle-2d-to-named ()
  "[2, 4] -> [x=2, y=4]."
  (my-calc-coordinate-toggle-test "[2, 4]" "[x=2, y=4]"))

(ert-deftest test-my/calc-coordinate-toggle-3d-to-named ()
  "[1, 2, 3] -> [x=1, y=2, z=3]."
  (my-calc-coordinate-toggle-test "[1, 2, 3]" "[x=1, y=2, z=3]"))

(ert-deftest test-my/calc-coordinate-toggle-4d-to-named ()
  "[1, 2, 3, 4] -> [x=1, y=2, z=3, w=4]."
  (my-calc-coordinate-toggle-test "[1, 2, 3, 4]" "[x=1, y=2, z=3, w=4]"))


;;; xyzw → hklm

(ert-deftest test-my/calc-coordinate-toggle-xyzw-to-hklm ()
  "[x=2, y=4] -> [h=2, k=4]."
  (my-calc-coordinate-toggle-test "[x=2, y=4]" "[h=2, k=4]"))

(ert-deftest test-my/calc-coordinate-toggle-3d-xyzw-to-hklm ()
  "[x=1, y=2, z=3] -> [h=1, k=2, l=3]."
  (my-calc-coordinate-toggle-test "[x=1, y=2, z=3]" "[h=1, k=2, l=3]"))


;;; hklm → pqrs

(ert-deftest test-my/calc-coordinate-toggle-hklm-to-pqrs ()
  "[h=2, k=4] -> [p=2, q=4]."
  (my-calc-coordinate-toggle-test "[h=2, k=4]" "[p=2, q=4]"))

(ert-deftest test-my/calc-coordinate-toggle-3d-hklm-to-pqrs ()
  "[h=1, k=2, l=3] -> [p=1, q=2, r=3]."
  (my-calc-coordinate-toggle-test "[h=1, k=2, l=3]" "[p=1, q=2, r=3]"))


;;; pqrs → xyzw

(ert-deftest test-my/calc-coordinate-toggle-pqrs-to-xyzw ()
  "[p=2, q=4] -> [x=2, y=4]."
  (my-calc-coordinate-toggle-test "[p=2, q=4]" "[x=2, y=4]"))

(ert-deftest test-my/calc-coordinate-toggle-3d-pqrs-to-xyzw ()
  "[p=1, q=2, r=3] -> [x=1, y=2, z=3]."
  (my-calc-coordinate-toggle-test "[p=1, q=2, r=3]" "[x=1, y=2, z=3]"))


;;; Round-trip (xyzw → hklm → pqrs → xyzw)

(ert-deftest test-my/calc-coordinate-toggle-round-trip ()
  "[x=2, y=4] -> [h=2, k=4] -> [p=2, q=4] -> [x=2, y=4]."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "[x=2, y=4]"))
    (my/calc-coordinate-toggle)
    (my/calc-coordinate-toggle)
    (my/calc-coordinate-toggle)
    (should (equal (car (nth 1 calc-stack))
                   (math-read-expr "[x=2, y=4]")))))


(provide 'my-calc-coordinate-toggle-tests)
