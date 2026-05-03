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

;;; Integer-coefficient tests

(ert-deftest test-my/calc-poly-lcm-integer-coeff ()
  "lcm(6*(x+1), 4*(x+1)) = 12*(x+1): integer content extracted correctly."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-poly-lcm-tests--lcm "6*(x+1)" "4*(x+1)")))
      (should (my-calc-poly-lcm-tests--equiv result "12*(x+1)")))))

(ert-deftest test-my/calc-poly-lcm-integer-coeff-expanded-input ()
  "lcm(6*x+6, 4*x+4) = 12*(x+1): works with expanded input too."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-poly-lcm-tests--lcm "6*x+6" "4*x+4")))
      (should (my-calc-poly-lcm-tests--equiv result "12*(x+1)")))))

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

;;; Multivariate tests

(ert-deftest test-my/calc-poly-lcm-multivariate ()
  "lcm(4*(z-4)^2*(w+3), 18*(z-4)^3*(w+3)^3) = 36*(z-4)^3*(w+3)^3."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-poly-lcm-tests--lcm "4*(z-4)^2*(w+3)" "18*(z-4)^3*(w+3)^3")))
      (should (my-calc-poly-lcm-tests--equiv result "36*(z-4)^3*(w+3)^3")))))

(ert-deftest test-my/calc-poly-lcm-multivariate-mixed-exponents ()
  "lcm(12*z^6*(w-7)^3, 20*z^5*(w-7)^4) = 60*z^6*(w-7)^4."
  (with-temp-buffer
    (calc-mode)
    (let ((result (my-calc-poly-lcm-tests--lcm "12*z^6*(w-7)^3" "20*z^5*(w-7)^4")))
      (should (my-calc-poly-lcm-tests--equiv result "60*z^6*(w-7)^4")))))

(provide 'my-calc-poly-lcm-tests)
