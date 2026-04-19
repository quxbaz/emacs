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


;;; Named form → plain vector

(ert-deftest test-my/calc-coordinate-toggle-2d-to-plain ()
  "[x=2, y=4] -> [2, 4]."
  (my-calc-coordinate-toggle-test "[x=2, y=4]" "[2, 4]"))

(ert-deftest test-my/calc-coordinate-toggle-3d-to-plain ()
  "[x=1, y=2, z=3] -> [1, 2, 3]."
  (my-calc-coordinate-toggle-test "[x=1, y=2, z=3]" "[1, 2, 3]"))


;;; Round-trip

(ert-deftest test-my/calc-coordinate-toggle-round-trip ()
  "[2, 4] -> [x=2, y=4] -> [2, 4]."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "[2, 4]"))
    (my/calc-coordinate-toggle)
    (my/calc-coordinate-toggle)
    (should (equal (car (nth 1 calc-stack))
                   (math-read-expr "[2, 4]")))))


(provide 'my-calc-coordinate-toggle-tests)
