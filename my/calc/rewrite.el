;; -*- lexical-binding: t; -*-
;;
;; Calc rewrite rules and transformations


(defun my/calc-to-degrees ()
  "Converts radian value to degree."
  (interactive)
  (let ((rules (list "r := r * 180 / pi")))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))

(defun my/calc-to-radians ()
  "Converts degree value to radian as a factor of pi."
  (interactive)
  (let ((rules (list "d := d * pi / 180")))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))

(defun my/calc-mod-360 ()
  "Applies modulo 360 (degrees). If hyperbolic flag is t, use 180 (degrees)
as the modulus."
  (interactive)
  (calc-wrapper
   (let ((modulus (if (calc-is-hyperbolic) 180 360)))
     (calc-enter-result 1 "norm" (math-mod (calc-top-n 1) modulus)))))

(defun my/calc-log-exp-rules ()
  "Applies logarithm and exponential identities.

Applies the following rules:
  - b^log(x, b) = x             (exponential-logarithm identity)
  - b^(-log(x, b)) = 1/x        (negative exponent variant)
  - e^ln(x) = x                 (natural base identity)
  - e^(-ln(x)) = 1/x            (natural base negative exponent)
  - 10^log10(x) = x             (base-10 identity)
  - 10^(-log10(x)) = 1/x        (base-10 negative exponent)
  - ln(x^p) = p * ln(x)         (logarithm power rule)
  - log(x^p, b) = p * log(x, b) (logarithm power rule)
  - log10(x^p) = p * log10(x)   (base-10 power rule)"
  (interactive)
  (let ((rules (list "b^log(x, b) := x"
                     "b^(-log(x, b)) := 1/x"
                     "e^ln(x) := x"
                     "e^(-ln(x)) := 1/x"
                     "10^log10(x) := x"
                     "10^(-log10(x)) := 1/x"
                     "ln(x^p) := p * ln(x)"
                     "log(x^p, b) := p * log(x, b)"
                     "log10(x^p) := p * log10(x)")))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))

(defun my/calc-collect-fractions ()
  "Collect additive terms into a single fraction over their LCD.
Works on the active selection or the top stack entry.

Two distinct behaviors depending on the expression shape:
- Sum/difference of fractions: combines all additive terms over their LCD.
    e.g. a/2 + b/3  =>  (3a + 2b) / 6
- Calc-internal (frac p q) / e: flattens the stacked division into a single one.
    e.g. (3/4) / x  =>  3 / (4*x)
  This arises when Calc represents an integer-ratio literal divided by something."
  (interactive)
  (cl-labels
      ;; Flatten a sum/difference tree into a list of signed terms.
      ;; Subtracted sub-expressions are negated so all terms carry their sign.
      ((terms (expr)
         (cond ((eq (car-safe expr) '+)
                (append (terms (nth 1 expr)) (terms (nth 2 expr))))
               ((eq (car-safe expr) '-)
                (append (terms (nth 1 expr))
                        (mapcar #'math-neg (terms (nth 2 expr)))))
               (t (list expr))))
       ;; Calc sometimes wraps integer parts of a rational inside a `cplx'
       ;; node; unwrap it so GCD and LCM operate on plain integers.
       (to-int (x)
         (if (eq (car-safe x) 'cplx) (nth 1 x) x))
       ;; Return the numerator of a term.  Non-fraction terms are their own numerator.
       (numer (term)
         (if (memq (car-safe term) '(/ frac)) (nth 1 term) term))
       ;; Return the denominator of a term as a plain integer.
       ;; Non-fraction terms have an implicit denominator of 1.
       (denom (term)
         (to-int (if (memq (car-safe term) '(/ frac)) (nth 2 term) 1)))
       (my-lcm (a b)
         (let ((g (math-gcd a b)))
           (math-mul (math-div a g) b)))
       (transform (expr)
         ;; Special case: Calc stores literal rationals as `(frac p q)'.
         ;; When such a node appears as the numerator of a division — e.g.
         ;; `(/ (frac 3 4) x)' for "(3/4)/x" — flatten it to `(/ p (* q e))'.
         (if (and (eq (car-safe expr) '/)
                  (eq (car-safe (nth 1 expr)) 'frac))
             (let* ((fr (nth 1 expr)) (p (nth 1 fr)) (q (nth 2 fr)) (e (nth 2 expr)))
               (calc-normalize (list '/ p (math-mul q e))))
           ;; General case: collect all additive terms over their LCD.
           ;; Each term is rescaled by (lcd / its-own-denom) so the
           ;; denominators cancel and the result is a single fraction.
           (let* ((ts  (terms expr))
                  (lcd (cl-reduce #'my-lcm (mapcar #'denom ts) :initial-value 1))
                  (num (cl-reduce #'math-add
                                  (mapcar (lambda (term)
                                            (math-mul (numer term)
                                                      (math-div lcd (denom term))))
                                          ts)
                                  :initial-value 0)))
             (calc-normalize (list '/ num lcd))))))
    (my/calc-replace-expr-dwim (expr replace-expr) ((prefix "cltf") (simp -1))
      (replace-expr (transform expr)))))

(defun my/calc-complete-the-square ()
  "Completes the square for quadratic expressions and equations.

Handles forms like:
    x^2 + bx          ->  (x + b/2)^2 - (b/2)^2
    ax^2 + bx         ->  a*((x + b/2a)^2 - (b/2a)^2)
    ax^2 + bx = y     ->  (x + b/2a)^2 - (b/2a)^2 = y/a"
  (interactive)
  (let ((rules (list
                ;; x^2 ± x
                "x^2 + x := (x + 1/2)^2 - (1/4) :: variable(x)"
                "x^2 - x := (x - 1/2)^2 - (1/4) :: variable(x)"
                ;; x^2 ± bx
                "x^2 + b*x := (x + b/2)^2 - (b/2)^2 :: variable(x)"
                "x^2 - b*x := (x - b/2)^2 - (b/2)^2 :: variable(x)"
                ;; ax^2 ± x = y
                "a*x^2 + x = y := (x + 1/2a)^2 - (1/2a)^2 = y/a :: variable(x)"
                "a*x^2 - x = y := (x - 1/2a)^2 - (1/2a)^2 = y/a :: variable(x)"
                ;; ax^2 ± bx = y
                "a*x^2 + b*x = y := (x + b/2a)^2 - (b/2a)^2 = y/a :: variable(x)"
                "a*x^2 - b*x = y := (x - b/2a)^2 - (b/2a)^2 = y/a :: variable(x)"
                ;; ax^2 ± x (factored)
                "a*x^2 + x := a * ((x + 1/2*a)^2 - (1/2*a)^2) :: variable(x)"
                "a*x^2 - x := a * ((x - 1/2*a)^2 - (1/2*a)^2) :: variable(x)"
                ;; ax^2 ± bx (factored)
                "a*x^2 + b*x := a * ((x + b/2*a)^2 - (b/2*a)^2) :: variable(x)"
                "a*x^2 - b*x := a * ((x - b/2*a)^2 - (b/2*a)^2) :: variable(x)"
                )))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))

(defun my/calc-factor-powers ()
  "Factors expressions as sums/differences of squares and cubes.

Handles forms like:
    a^2 - b^2         ->  (a + b)(a - b)
    a^2 + b^2         ->  (b + a*i)(b - a*i)
    a^3 + b^3         ->  (a + b)(a^2 - ab + b^2)
    a^3 - b^3         ->  (a - b)(a^2 + ab + b^2)

Uses complex numbers for sums of squares, cube roots for non-cube terms,
and handles coefficients."
  (interactive)
  (let ((rules (list
                ;; Squares: a^2 - b^2
                "a^2 - b^2 := (a + b)(a - b)"
                ;; Squares: a^2 + b^2 (complex)
                "a^2 + b^2 := (b + a*i)(b - a*i)"
                ;; Squares: a^2 - b
                "a^2 - b := (a + sqrt(b))(a - sqrt(b))"
                ;; Squares: a^2 + b (complex)
                "a^2 + b := (a + sqrt(b)*i)(a - sqrt(b)*i)"
                ;; Squares: c*a^2 - b
                "c * a^2 - b := c * (a + sqrt(b/c))(a - sqrt(b/c))"
                ;; Squares: c*a^2 + b (complex)
                "c * a^2 + b := c * (a + sqrt(b/c)*i)(a - sqrt(b/c)*i)"
                ;; Squares: b - a^2 (reverse order)
                "b - a^2 := (sqrt(b) + a)(sqrt(b) - a)"
                ;; Squares: b - c*a^2 (reverse order with coefficient)
                "b - c*a^2 := (sqrt(b) + sqrt(c)*a)(sqrt(b) - sqrt(c)*a)"
                ;; Cubes: a^3 + b^3
                "plain(a^3 + b^3) := (a + b)(a^2 - a*b + b^2)"
                ;; Cubes: a^3 - b^3
                "plain(a^3 - b^3) := (a - b)(a^2 + a*b + b^2)"
                ;; Cubes: a^3 + b (with cube root)
                "plain(a^3 + b) := (a + b^1:3)(a^2 - a*b^1:3 + b^2:3)"
                ;; Cubes: a^3 - b (with cube root)
                "plain(a^3 - b) := (a - b^1:3)(a^2 + a*b^1:3 + b^2:3)"
                ;; Cubes: c*a^3 + b^3
                "plain(c*a^3 + b^3) := ((c^1:3)*a + b)((c^2:3)*a^2 - (c^1:3)*a*b + b^2)"
                ;; Cubes: c*a^3 - b^3
                "plain(c*a^3 - b^3) := ((c^1:3)*a - b)((c^2:3)*a^2 + (c^1:3)*a*b + b^2)"
                ;; Cubes: c*a^3 + d (both with cube roots)
                "plain(c*a^3 + d) := ((c^1:3)*a + d^1:3)((c^2:3)*a^2 - (c^1:3)*(d^1:3)*a + d^2:3)"
                ;; Cubes: c*a^3 - d (both with cube roots)
                "plain(c*a^3 - d) := ((c^1:3)*a - d^1:3)((c^2:3)*a^2 + (c^1:3)*(d^1:3)*a + d^2:3)"
                ;; Cubes: a^m/c + b^n/d where m, n are multiples of 3 (e.g. a^6/8 + b^3/8)
                "plain(a^m/c + b^n/d) := (a^(m/3)/(c^(1:3)) + b^(n/3)/(d^(1:3)))(a^(2*m/3)/(c^(2:3)) - a^(m/3)*b^(n/3)/((c^(1:3))*(d^(1:3))) + b^(2*n/3)/(d^(2:3))) :: m % 3 = 0 && n % 3 = 0"
                ;; Cubes: a^m/c - b^n/d where m, n are multiples of 3 (e.g. a^6/8 - b^3/8)
                "plain(a^m/c - b^n/d) := (a^(m/3)/(c^(1:3)) - b^(n/3)/(d^(1:3)))(a^(2*m/3)/(c^(2:3)) + a^(m/3)*b^(n/3)/((c^(1:3))*(d^(1:3))) + b^(2*n/3)/(d^(2:3))) :: m % 3 = 0 && n % 3 = 0"
                ;; Cubes: a^m/c + b^n where m, n are multiples of 3 (e.g. a^6/8 + b^3)
                "plain(a^m/c + b^n) := (a^(m/3)/(c^(1:3)) + b^(n/3))(a^(2*m/3)/(c^(2:3)) - a^(m/3)*b^(n/3)/(c^(1:3)) + b^(2*n/3)) :: m % 3 = 0 && n % 3 = 0"
                ;; Cubes: a^m/c - b^n where m, n are multiples of 3 (e.g. a^6/8 - b^3)
                "plain(a^m/c - b^n) := (a^(m/3)/(c^(1:3)) - b^(n/3))(a^(2*m/3)/(c^(2:3)) + a^(m/3)*b^(n/3)/(c^(1:3)) + b^(2*n/3)) :: m % 3 = 0 && n % 3 = 0"
                ;; Cubes: a^m + b^n/c where m, n are multiples of 3 (e.g. x^6 + y^6/8)
                "plain(a^m + b^n/c) := (a^(m/3) + b^(n/3)/(c^(1:3)))(a^(2*m/3) - a^(m/3)*b^(n/3)/(c^(1:3)) + b^(2*n/3)/(c^(2:3))) :: m % 3 = 0 && n % 3 = 0 && m > 3"
                ;; Cubes: a^m - b^n/c where m, n are multiples of 3 (e.g. x^6 - y^6/8)
                "plain(a^m - b^n/c) := (a^(m/3) - b^(n/3)/(c^(1:3)))(a^(2*m/3) + a^(m/3)*b^(n/3)/(c^(1:3)) + b^(2*n/3)/(c^(2:3))) :: m % 3 = 0 && n % 3 = 0 && m > 3"
                ;; Cubes: a^m + b^n where m, n are multiples of 3 (e.g. x^6 + y^6)
                "plain(a^m + b^n) := (a^(m/3) + b^(n/3))(a^(2*m/3) - a^(m/3)*b^(n/3) + b^(2*n/3)) :: m % 3 = 0 && n % 3 = 0 && m > 3"
                ;; Cubes: a^m - b^n where m, n are multiples of 3 (e.g. x^6 - y^6)
                "plain(a^m - b^n) := (a^(m/3) - b^(n/3))(a^(2*m/3) + a^(m/3)*b^(n/3) + b^(2*n/3)) :: m % 3 = 0 && n % 3 = 0 && m > 3"
                ;; Cubes: b - a^m (constant first, e.g. 64 - y^6)
                "plain(b - a^m) := (b^(1:3) - a^(m/3))*(b^(2:3) + b^(1:3)*a^(m/3) + a^(2*m/3)) :: m % 3 = 0 && m >= 3"
                ;; Cubes: b + a^m (constant first, e.g. 64 + y^6)
                "plain(b + a^m) := (b^(1:3) + a^(m/3))*(b^(2:3) - b^(1:3)*a^(m/3) + a^(2*m/3)) :: m % 3 = 0 && m >= 3"
                ;; Cubes: c - d*a^m (constant minus coefficient×power, e.g. 256 - 4*y^6)
                "plain(c - d*a^m) := d*((c/d)^(1:3) - a^(m/3))*((c/d)^(2:3) + (c/d)^(1:3)*a^(m/3) + a^(2*m/3)) :: m % 3 = 0 && m >= 3"
                ;; Cubes: c + d*a^m (constant plus coefficient×power, e.g. 256 + 4*y^6)
                "plain(c + d*a^m) := d*((c/d)^(1:3) + a^(m/3))*((c/d)^(2:3) - (c/d)^(1:3)*a^(m/3) + a^(2*m/3)) :: m % 3 = 0 && m >= 3"
                ;; Higher powers: a^n as difference of squares
                "plain(a^n - b) := ((a^(n/2)) + sqrt(b))((a^(n/2)) - sqrt(b)) :: n > 3"
                "plain(a^n + b) := ((a^(n/2)) + sqrt(b)*i)((a^(n/2)) - sqrt(b)*i) :: n > 3"
                ;; Higher powers: c*a^m - b^n as difference of squares
                "plain(c*a^m - b^n) := (sqrt(c)*a^(m/2) + b^(m/2))(sqrt(c)*a^(m/2) - b^(m/2)) :: m > 3"
                ;; Higher powers: c*a^m - d*b^n as difference of squares
                "plain(c*a^m - d*b^n) := (sqrt(c)*a^(m/2) + sqrt(d)*b^(n/2))(sqrt(c)*a^(m/2) - sqrt(d)*b^(n/2)) :: m > 3"
                )))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))

(provide 'my/calc/rewrite)
