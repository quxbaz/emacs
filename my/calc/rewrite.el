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
  "Applies modulo 360 (degrees)."
  (interactive)
  (calc-wrapper
   (calc-enter-result 1 "norm" (math-mod (calc-top-n 1) 360))))

(defun my/calc-log-exp-rules ()
  "Applies logarithm and exponential identities.

Applies the following rules:
  - b^log(x, b) = x             (exponential-logarithm identity)
  - ln(x^p) = p * ln(x)         (logarithm power rule)
  - log(x^p, b) = p * log(x, b) (logarithm power rule)"
  (interactive)
  (let ((rules (list "b^log(x, b) := x"
                     "ln(x^p) := p * ln(x)"
                     "log(x^p, b) := p * log(x, b)")))
    (calc-wrapper
     (calc-rewrite (s-join "," rules) 1))))

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
