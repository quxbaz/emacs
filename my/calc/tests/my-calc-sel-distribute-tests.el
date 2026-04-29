;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-sel-distribute

(require 'ert)
(require 'calc)
(require 'calc-ext)

(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/selection.el" user-emacs-directory))


;;; Helper

(defmacro with-dist-buffer (expr-str &rest body)
  "Push EXPR-STR with the second arg (sum) selected; eval BODY; return the result."
  (declare (indent 1))
  `(with-temp-buffer
     (calc-mode)
     (calc-reset 0)
     (setq calc-symbolic-mode t
           calc-prefer-frac t
           calc-use-selections t)
     (let ((calc-simplify-mode 'none))
       (let* ((expr (math-read-expr ,expr-str)))
         (calc-push expr)
         ;; Select the sum (second arg of *) so calc-sel-distribute distributes.
         (setcar (nthcdr 2 (nth 1 calc-stack)) (nth 2 expr))
         ,@body
         (car (nth 1 calc-stack))))))

(defun dist-equiv-p (result expected-str)
  "Return t if RESULT is algebraically equal to EXPECTED-STR."
  (math-zerop (math-simplify (math-sub result (math-read-expr expected-str)))))


;;; Basic distribution

(ert-deftest test-calc-sel-distribute-basic ()
  "a*(b+c) distributes to a*b+a*c."
  (should (dist-equiv-p (with-dist-buffer "a * (b + c)"
                          (my/calc-sel-distribute))
                        "a*b + a*c")))

(ert-deftest test-calc-sel-distribute-symbolic-coefficients ()
  "k*(x+y) distributes to k*x+k*y."
  (should (dist-equiv-p (with-dist-buffer "k * (x + y)"
                          (my/calc-sel-distribute))
                        "k*x + k*y")))


;;; No simplification — numeric products must not be evaluated

(ert-deftest test-calc-sel-distribute-no-simplify-mixed ()
  "2*(3+x) distributes to 2*3+2*x, not 6+2*x."
  (let* ((result (with-dist-buffer "2 * (3 + x)"
                   (my/calc-sel-distribute)))
         (s (math-format-value result)))
    ;; "2 3" means the product 2*3 was left unsimplified.
    (should (string-match-p "2 3" s))))

(ert-deftest test-calc-sel-distribute-no-simplify-numeric ()
  "2*(3+4) distributes to 2*3+2*4, not 14."
  (let* ((result (with-dist-buffer "2 * (3 + 4)"
                   (my/calc-sel-distribute)))
         (s (math-format-value result)))
    (should (string-match-p "2 3" s))
    (should (string-match-p "2 4" s))))
