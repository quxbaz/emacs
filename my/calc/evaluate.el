;; -*- lexical-binding: t; -*-
;;
;; my/calc-evaluate (k k) and my/calc--identify-expr (I k k)

(require 'my/calc/lib)

(defun my/calc--identify-expr (x)
  "Try to identify X as a simple closed-form expression.
Tries in order: integer, fraction p/q (|q|≤20), (p/q)·√n for
square-free n≤30, √n, n^(1/3), n^(1/4), (p/q)·π, (p/q)·e, ln(n)
(n≤1000).  Signals an error if no candidate matches within 1e-8."
  (let* ((calc-symbolic-mode nil)
         (xf  (math-evaluate-expr x))
         (neg (math-negp xf))
         (ax  (if neg (math-neg xf) xf))
         (tol '(float 1 -8))
         ;; Square-free integers 2..30 (no repeated prime factor).
         ;; Keeps √n candidates irreducible so e.g. √6 isn't misidentified
         ;; as (1/2)·√24.
         (sqfree-ns '(2 3 5 6 7 10 11 13 14 15 17 19 21 22 23 26 29 30)))
    (cl-flet ((close-p (sym)
                (math-lessp
                 (math-abs (math-sub (math-evaluate-expr sym) ax)) tol))
              (maybe-neg (e) (if neg (math-neg e) e))
              (sym-normalize (e)
                ;; Normalize symbolic form without evaluating calc functions
                ;; to floats (e.g. (1/2)·√3 must not collapse to a float).
                (let ((calc-symbolic-mode t)) (math-normalize e)))
              (try-rat (af)
                (cl-loop for q from 1 to 20 thereis
                         (let* ((pf (math-mul af q))
                                (p  (calcFunc-round pf)))
                           (and (math-lessp (math-abs (math-sub pf p)) tol)
                                (if (= q 1) p
                                  (math-normalize (list 'frac p q))))))))
      (or
       ;; 1. Integer
       (let ((r (calcFunc-round ax)))
         (and (close-p r) (maybe-neg r)))
       ;; 2. Simple fraction p/q
       (let ((r (try-rat ax)))
         (and r (maybe-neg r)))
       ;; 3. (p/q)·√n for square-free n (ratio ≠ 1 to avoid duplicating case 4)
       (cl-loop for n in sqfree-ns thereis
                (let* ((sn (math-evaluate-expr (list 'calcFunc-sqrt n)))
                       (r  (try-rat (math-div ax sn))))
                  (and r (not (math-equal r 1))
                       (maybe-neg
                        (sym-normalize (list '* r (list 'calcFunc-sqrt n)))))))
       ;; 4. √n
       (let* ((sq (math-evaluate-expr (list 'calcFunc-sqr ax)))
              (n  (calcFunc-round sq)))
         (and (close-p (list 'calcFunc-sqrt n))
              (math-posp n) (not (math-equal n 1))
              (maybe-neg (list 'calcFunc-sqrt n))))
       ;; 5. n^(1/3)
       (let* ((cb (math-evaluate-expr (list '^ ax 3)))
              (n  (calcFunc-round cb)))
         (and (close-p (list '^ n (list 'frac 1 3)))
              (math-posp n) (not (math-equal n 1))
              (maybe-neg (list '^ n (list 'frac 1 3)))))
       ;; 6. n^(1/4)
       (let* ((qt (math-evaluate-expr (list '^ ax 4)))
              (n  (calcFunc-round qt)))
         (and (close-p (list '^ n (list 'frac 1 4)))
              (math-posp n) (not (math-equal n 1))
              (maybe-neg (list '^ n (list 'frac 1 4)))))
       ;; 7. (p/q)·π
       (let ((r (try-rat (math-div ax (math-evaluate-expr '(var pi var-pi))))))
         (and r (maybe-neg (sym-normalize (list '* r '(var pi var-pi))))))
       ;; 8. (p/q)·e
       (let ((r (try-rat (math-div ax (math-evaluate-expr '(var e var-e))))))
         (and r (maybe-neg (sym-normalize (list '* r '(var e var-e))))))
       ;; 9. ln(n) — only for x > 0
       (and (not neg)
            (let* ((en (math-evaluate-expr (list 'calcFunc-exp ax)))
                   (n  (calcFunc-round en)))
              (and (close-p (list 'calcFunc-ln n))
                   (>= (math-compare n 2) 0)
                   (<= (math-compare n 1000) 0)
                   (list 'calcFunc-ln n))))
       (error "Cannot identify %s as a simple expression"
              (math-format-value xf))))))

(defun my/calc-evaluate (n)
  "Evaluate the target expression with symbolic mode disabled.
With inverse (I k k): identify the expression as a simple closed-form
(integer, fraction, sqrt, cbrt, π/e multiple, ln…), keeping the result
symbolic.  Works contextually: operates on selection, sub-formula at
point, or stack entry at level N (default 1)."
  (interactive "p")
  (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "eval") (m n))
    (unwind-protect
        (if (calc-is-inverse)
            (let ((calc-symbolic-mode t))
              (replace-expr (my/calc--identify-expr expr)))
          (let ((calc-symbolic-mode nil))
            (replace-expr (math-evaluate-expr expr))))
      (calc-set-mode-line))))

(provide 'my/calc/evaluate)
