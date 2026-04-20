;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-poly-lcm
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))

(defun my-calc-poly-lcm-tests--lcm (a-str b-str)
  "Push A-STR and B-STR onto the stack, run my/calc-poly-lcm, return result."
  (calc-reset 0)
  (calc-push (math-read-expr a-str))
  (calc-push (math-read-expr b-str))
  (my/calc-poly-lcm)
  (car (nth 1 calc-stack)))

(defun my-calc-poly-lcm-tests--equiv (actual expected-str)
  "Return t if ACTUAL equals EXPECTED-STR after expanding both."
  (let* ((expected (math-read-expr expected-str))
         (diff (math-normalize (list 'calcFunc-expand (list '- actual expected)))))
    (math-zerop (math-simplify diff))))

(defun my-calc-poly-lcm-tests--factored-p (expr)
  "Return t if EXPR is a product (i.e. factoring occurred)."
  (eq (car-safe expr) '*))

;;; Basic tests

(ert-deftest test-my/calc-poly-lcm-simple ()
  "lcm(x^2 - 1, x^2 - x) = x*(x+1)*(x-1)."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-poly-lcm-tests--lcm "x^2 - 1" "x^2 - x")))
      (should (my-calc-poly-lcm-tests--equiv result "x*(x+1)*(x-1)")))))

(ert-deftest test-my/calc-poly-lcm-simple-is-factored ()
  "lcm(x^2 - 1, x^2 - x) result is a product."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-poly-lcm-tests--lcm "x^2 - 1" "x^2 - x")))
      (should (my-calc-poly-lcm-tests--factored-p result)))))

(ert-deftest test-my/calc-poly-lcm-coprime ()
  "lcm(x+1, x+2) = (x+1)*(x+2) when coprime."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-poly-lcm-tests--lcm "x + 1" "x + 2")))
      (should (my-calc-poly-lcm-tests--equiv result "(x+1)*(x+2)")))))

(ert-deftest test-my/calc-poly-lcm-identical ()
  "lcm(x^2 - 1, x^2 - 1) = x^2 - 1."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-poly-lcm-tests--lcm "x^2 - 1" "x^2 - 1")))
      (should (my-calc-poly-lcm-tests--equiv result "x^2 - 1")))))

(provide 'my-calc-poly-lcm-tests)
