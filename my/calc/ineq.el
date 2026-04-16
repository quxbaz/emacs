;; -*- lexical-binding: t; -*-
;;
;; Calc inequality operations


(defun my/calc-abs-ineq--first-var (expr)
  "Return the first Calc variable found in EXPR, or nil."
  (cond
   ((eq (car-safe expr) 'var) expr)
   ((listp expr) (cl-some #'my/calc-abs-ineq--first-var (cdr expr)))
   (t nil)))

(defun my/calc-abs-ineq--solve (ineq var)
  "Solve inequality INEQ for VAR using Calc's solver.
Returns INEQ unchanged if VAR is nil, if VAR is already alone on one
side, or if solving fails."
  (cond
   ((null var) ineq)
   ((or (equal (nth 1 ineq) var)
        (equal (nth 2 ineq) var))
    ineq)
   (t (or (math-solve-eqn ineq var nil) ineq))))

(defun my/calc-abs-ineq--flip (ineq)
  "Flip INEQ: swap operands and reverse operator."
  (list (pcase (car ineq)
          ('calcFunc-lt  'calcFunc-gt)
          ('calcFunc-leq 'calcFunc-geq)
          ('calcFunc-gt  'calcFunc-lt)
          ('calcFunc-geq 'calcFunc-leq))
        (nth 2 ineq)
        (nth 1 ineq)))

(defun my/calc-abs-ineq--land (p1 p2 var)
  "Combine P1 && P2, ordering so the lower bound (VAR on RHS) comes first.
This produces the ergonomic form: lower < VAR && VAR < upper."
  (if (and var (equal (nth 1 p1) var))
      (list 'calcFunc-land p2 p1)
    (list 'calcFunc-land p1 p2)))

(defun my/calc-abs-ineq--lor (p1 p2 var)
  "Combine P1 || P2 with VAR on LHS of each, left tail first.
This produces the ergonomic form: VAR < lower || VAR > upper."
  (let* ((n1 (if (and var (equal (nth 2 p1) var))
                 (my/calc-abs-ineq--flip p1) p1))
         (n2 (if (and var (equal (nth 2 p2) var))
                 (my/calc-abs-ineq--flip p2) p2))
         (lt-first (memq (car n1) '(calcFunc-lt calcFunc-leq))))
    (if lt-first
        (list 'calcFunc-lor n1 n2)
      (list 'calcFunc-lor n2 n1))))

(defun my/calc-abs-ineq ()
  "Converts absolute value inequalities into compound inequalities,
solving each part for the variable in a where possible.

Handles forms like:
    |a| < b    ->  lower < a && a < upper  (solved for var in a)
    |a| <= b   ->  lower <= a && a <= upper
    |a| > b    ->  a < lower || a > upper
    |a| >= b   ->  a <= lower || a >= upper"
  (interactive)
  (calc-wrapper
   (let* ((expr (calc-top-n 1))
          (op   (car-safe expr))
          (lhs  (nth 1 expr))
          (rhs  (nth 2 expr)))
     (when (and (memq op '(calcFunc-lt calcFunc-leq calcFunc-gt calcFunc-geq))
                (eq (car-safe lhs) 'calcFunc-abs))
       (let* ((a     (nth 1 lhs))
              (b     rhs)
              (neg-b (math-neg b))
              (var   (my/calc-abs-ineq--first-var a))
              (result
               (pcase op
                 ('calcFunc-lt
                  (my/calc-abs-ineq--land
                   (my/calc-abs-ineq--solve (list 'calcFunc-lt neg-b a) var)
                   (my/calc-abs-ineq--solve (list 'calcFunc-lt a b) var)
                   var))
                 ('calcFunc-leq
                  (my/calc-abs-ineq--land
                   (my/calc-abs-ineq--solve (list 'calcFunc-leq neg-b a) var)
                   (my/calc-abs-ineq--solve (list 'calcFunc-leq a b) var)
                   var))
                 ('calcFunc-gt
                  (my/calc-abs-ineq--lor
                   (my/calc-abs-ineq--solve (list 'calcFunc-lt a neg-b) var)
                   (my/calc-abs-ineq--solve (list 'calcFunc-gt a b) var)
                   var))
                 ('calcFunc-geq
                  (my/calc-abs-ineq--lor
                   (my/calc-abs-ineq--solve (list 'calcFunc-leq a neg-b) var)
                   (my/calc-abs-ineq--solve (list 'calcFunc-geq a b) var)
                   var)))))
         (calc-enter-result 1 "aiq" result))))))

(provide 'my/calc/ineq)
