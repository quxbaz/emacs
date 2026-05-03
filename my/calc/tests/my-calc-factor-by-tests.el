;; -*- lexical-binding: t; -*-
;;
;; Tests for my/calc-factor-by, my/calc-factor-by-gcd, and my/calc-replace-expr-dwim
;;

(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'cl-lib)

;; Load the code under test
(load-file (expand-file-name "my/calc/stack.el" user-emacs-directory))
(load-file (expand-file-name "my/calc/lib.el" user-emacs-directory))

;;; Macro expansion tests

(ert-deftest test-my/calc-replace-expr-dwim-expands ()
  "Test that the macro expands without errors."
  (should (macroexpand '(my/calc-replace-expr-dwim (expr replace-expr) ((m 2) (prefix "test"))
                          (message "test")))))

(ert-deftest test-my/calc-replace-expr-dwim-generates-cl-flet ()
  "Test that the macro generates cl-flet, not fset."
  (let ((expansion (macroexpand '(my/calc-replace-expr-dwim (expr replace-expr) ((m 2))
                                   (replace-expr 'foo)))))
    ;; Should contain cl-flet
    (should (string-match-p "cl-flet" (format "%S" expansion)))
    ;; Should NOT contain fset
    (should-not (string-match-p "fset" (format "%S" expansion)))))

;;; Basic calc stack tests (without selections)

(ert-deftest test-my/calc-factor-by-simple ()
  "Test factoring 2x + 4 by 2 yields 2 * (x + 2)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    ;; Push 2x + 4 directly
    (calc-push (math-read-expr "2*x + 4"))
    ;; Push 2 (the factor)
    (calc-push 2)
    ;; Now stack has: 2x + 4, 2
    ;; Factor by 2
    (my/calc-factor-by)
    ;; Result should be 2 * (x + 2) - check internal structure
    (let ((result (car (nth 1 calc-stack))))
      ;; Result should be a product (* ...) containing factor 2
      (should (equal result '(* 2 (+ (var x var-x) 2)))))))

(ert-deftest test-my/calc-factor-by-numeric ()
  "Test factoring 6 by 3 yields 3 * 2."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    ;; Push 6
    (calc-push 6)
    ;; Push 3 (the factor)
    (calc-push 3)
    ;; Factor by 3
    (my/calc-factor-by)
    ;; Result should be 3 * 2 - check internal structure
    (let ((result (car (nth 1 calc-stack))))
      (print result)
      (should (equal result '(* 3 2))))))

(ert-deftest test-my/calc-factor-by-polynomial ()
  "Test factoring x^2 + 2x by x yields x * (x + 2)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    ;; Push x^2 + 2x
    (calc-push (math-read-expr "x^2 + 2*x"))
    ;; Push x (the factor)
    (calc-push (math-read-expr "x"))
    ;; Factor by x
    (my/calc-factor-by)
    ;; Result should be x * (x + 2) - check internal structure
    (let ((result (car (nth 1 calc-stack))))
      ;; Should be a product containing x
      (should (listp result))
      (should (eq (car result) '*))
      (should (member '(var x var-x) result)))))

;;; GCD factoring tests

(defun test-my/calc--contains-frac (expr)
  "Return t if EXPR contains any rational fraction sub-expressions."
  (cond ((eq (car-safe expr) 'frac) t)
        ((consp expr) (cl-some #'test-my/calc--contains-frac expr))
        (t nil)))

(ert-deftest test-my/calc-factor-by-gcd-basic ()
  "Test that 40*x^2 + 20*x is factored as 20*x*(2*x+1).
The GCD is 20*x; the result must not introduce fractions."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "40*x^2 + 20*x"))
    (my/calc-factor-by-gcd)
    (let* ((result (car (nth 1 calc-stack)))
           (expected (math-read-expr "20*x*(2*x+1)"))
           (diff (math-normalize (list '- result expected))))
      (should (math-zerop (math-simplify diff)))
      (should-not (test-my/calc--contains-frac result)))))

(ert-deftest test-my/calc-factor-by-gcd-basic-no-simplify-mode ()
  "Same as basic test but with calc-simplify-mode set to 'none.
Verifies the GCD computation is immune to the global simplify mode."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "40*x^2 + 20*x"))
    (let ((calc-simplify-mode 'none))
      (my/calc-factor-by-gcd))
    (let* ((result (car (nth 1 calc-stack)))
           (expected (math-read-expr "20*x*(2*x+1)"))
           (diff (math-normalize (list '- result expected))))
      (should (math-zerop (math-simplify diff)))
      (should-not (test-my/calc--contains-frac result)))))

;;; Edge cases

(ert-deftest test-my/calc-replace-expr-dwim-with-default-m ()
  "Test that m defaults to 1 when not specified."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 10)
    (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "test"))
      ;; expr should be the top of stack
      (should (equal expr 10)))))

(ert-deftest test-my/calc-replace-expr-dwim-with-custom-m ()
  "Test that custom m value is respected."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 10)  ; Stack level 2
    (calc-push 20)  ; Stack level 1 (top)
    (my/calc-replace-expr-dwim (expr replace-expr) ((m 2))
      ;; expr should be stack level 2 (which is 10)
      (should (equal expr 10)))))

(ert-deftest test-my/calc-replace-expr-dwim-binds-sel-is-active ()
  "Test that sel-is-active is bound correctly."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 42)
    (my/calc-replace-expr-dwim (expr replace-expr top sel-is-active) ()
      ;; sel-is-active should be nil (no selections)
      (should-not sel-is-active))))

(ert-deftest test-my/calc-replace-expr-dwim-replace-expr-callable ()
  "Test that replace-expr works as a function."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 10)
    (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "test"))
      ;; Call replace-expr to replace with 99
      ;; (Note: replace-expr is a cl-flet binding, not a variable, so functionp won't work)
      (replace-expr 99)
      ;; Top of stack should now be 99
      (should (equal (car (nth 1 calc-stack)) 99)))))

;;; Gensym tests

(ert-deftest test-my/calc-replace-expr-dwim-unused-bindings-gensym ()
  "Test that unused bindings are auto-gensymmed."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 42)
    ;; Only bind expr, let others be gensymmed
    (my/calc-replace-expr-dwim (expr) ()
      (should (equal expr 42)))))

(ert-deftest test-my/calc-replace-expr-dwim-no-bindings-gensym ()
  "Test that completely empty bindings use gensyms."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push 42)
    ;; All bindings gensymmed
    (my/calc-replace-expr-dwim () ()
      ;; Should still work, just can't reference the values
      (should (equal (car (nth 1 calc-stack)) 42)))))

;;; Selection tests

(ert-deftest test-my/calc-factor-by-selection-in-equation ()
  "Factor selected RHS of (y-2)^2 = -(4x)+16 by -4.
Expected: (y-2)^2 = -4*(x-4)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (let* ((full-expr (math-read-expr "(y-2)^2 = -(4*x) + 16"))
           (sel-expr  (nth 2 full-expr)))   ; RHS: -(4x) + 16
      (calc-push full-expr)
      (setf (nth 2 (nth 1 calc-stack)) sel-expr)
      (setq calc-use-selections t)
      (calc-push (math-read-expr "-4"))
      (my/calc-factor-by)
      (let* ((result (car (nth 1 calc-stack)))
             (lhs-diff (math-normalize
                        (list '- (nth 1 result) (math-read-expr "(y-2)^2"))))
             (rhs-diff (math-normalize
                        (list '- (nth 2 result) (math-read-expr "-4*(x-4)")))))
        (should (eq (car-safe result) 'calcFunc-eq))
        (should (math-zerop (math-simplify lhs-diff)))
        (should (math-zerop (math-simplify rhs-diff)))))))

;;; pop-stack tests

(ert-deftest test-my/calc-factor-by-equation-at-eol ()
  "Factor both sides of x^2+2x = 4x+8 by (x+2): result is x*(x+2) = 4*(x+2).
Verifies pop-stack=1 fires once — the factor is consumed and not left on the stack."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x^2 + 2*x = 4*x + 8"))
    (calc-push (math-read-expr "x + 2"))
    (calc-cursor-stack-index 2)  ; point on the equation
    (end-of-line)
    (my/calc-factor-by)
    ;; Only one item should remain (factor consumed)
    (should (= (calc-stack-size) 1))
    (let* ((result (car (nth 1 calc-stack)))
           (lhs-diff (math-normalize (list '- (nth 1 result) (math-read-expr "x*(x+2)"))))
           (rhs-diff (math-normalize (list '- (nth 2 result) (math-read-expr "4*(x+2)")))))
      (should (eq (car-safe result) 'calcFunc-eq))
      (should (math-zerop (math-simplify lhs-diff)))
      (should (math-zerop (math-simplify rhs-diff))))))

;;; Equation-mapping tests (map? option)

(ert-deftest test-my/calc-replace-expr-dwim-map-equation-at-eol ()
  "At eol on an equation, map?=t maps body over both sides.
Using my/calc-change-sign: x = y → -x = -y."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x = y"))
    (calc-cursor-stack-index 1)
    (end-of-line)
    (my/calc-change-sign)
    (let ((result (car (nth 1 calc-stack))))
      (should (eq (car-safe result) 'calcFunc-eq))
      (should (math-zerop (math-simplify
                            (math-add (nth 1 result) '(var x var-x)))))
      (should (math-zerop (math-simplify
                            (math-add (nth 2 result) '(var y var-y))))))))

(ert-deftest test-my/calc-replace-expr-dwim-map-inequality-at-eol ()
  "At eol on an inequality, map?=t maps body over both sides.
Using my/calc-change-sign: x < y → -x < -y (structural, not semantic)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x < y"))
    (calc-cursor-stack-index 1)
    (end-of-line)
    (my/calc-change-sign)
    (let ((result (car (nth 1 calc-stack))))
      (should (eq (car-safe result) 'calcFunc-lt))
      (should (math-zerop (math-simplify
                            (math-add (nth 1 result) '(var x var-x)))))
      (should (math-zerop (math-simplify
                            (math-add (nth 2 result) '(var y var-y))))))))

(ert-deftest test-my/calc-replace-expr-dwim-map-non-equation-at-eol ()
  "At eol on a non-equation expression, map?=t falls back to whole-entry behavior.
Using my/calc-change-sign: x+y at eol → -(x+y)."
  (with-temp-buffer
    (calc-mode)
    (calc-reset 0)
    (calc-push (math-read-expr "x + y"))
    (calc-cursor-stack-index 1)
    (end-of-line)
    (my/calc-change-sign)
    (let* ((result (car (nth 1 calc-stack)))
           (expected (math-read-expr "-(x+y)"))
           (diff (math-normalize (list '- result expected))))
      (should (math-zerop (math-simplify diff))))))

(provide 'factor-by-tests)
