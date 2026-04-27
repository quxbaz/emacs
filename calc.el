;;; Mode settings stored by Calc on Sat Apr 18 14:32:23 2026
(setq calc-symbolic-mode t)
(setq calc-prefer-frac t)
(setq calc-always-load-extensions t)
;;; End of mode settings

;;; Geometry — 2D
(setq var-eq-area-of-triangle '(calcFunc-eq (var A var-A) (* (frac 1 2) (* (var b var-b) (var h var-h)))))
(setq var-eq-general-area-of-triangle '(calcFunc-eq (var A var-A) (* (frac 1 2) (* (var a var-a) (* (var b var-b) (calcFunc-sin (var C var-C)))))))
(setq var-eq-area-of-trapezoid '(calcFunc-eq (var A var-A) (* (frac 1 2) (* (var h var-h) (+ (var b1 var-b1) (var b2 var-b2))))))
(setq var-eq-area-of-circular-sector '(calcFunc-eq (var A var-A) (* (frac 1 2) (* (^ (var r var-r) (cplx 2 0)) (var a var-a)))))
(setq var-eq-sum-of-interior-polygon-angles '(calcFunc-eq (var S var-S) (* 180 (- (var n var-n) 2))))
(setq var-eq-standard-equation-of-circle '(calcFunc-eq (+ (^ (- (var x var-x) (var h var-h)) 2) (^ (- (var y var-y) (var k var-k)) 2)) (^ (var r var-r) 2)))

;;; Geometry — 3D: Sphere
(setq var-eq-surface-area-of-sphere '(calcFunc-eq (var S var-S) (* 4 (* (var pi var-pi) (^ (var r var-r) 2)))))
(setq var-eq-volume-of-sphere '(calcFunc-eq (var V var-V) (* (frac 4 3) (* (var pi var-pi) (^ (var r var-r) 3)))))

;;; Geometry — 3D: Cylinder
(setq var-eq-surface-area-of-cylinder '(calcFunc-eq (var S var-S) (+ (* (cplx 2 0) (* (var pi var-pi) (* (var r var-r) (var h var-h)))) (* (cplx 2 0) (* (var pi var-pi) (^ (var r var-r) (cplx 2 0)))))))
(setq var-eq-lateral-surface-area-of-cylinder '(calcFunc-eq (var S var-S) (* 2 (* (var h var-h) (* (var pi var-pi) (var r var-r))))))
(setq var-eq-volume-of-cylinder '(calcFunc-eq (var V var-V) (* (var pi var-pi) (* (^ (var r var-r) (cplx 2 0)) (var h var-h)))))

;;; Geometry — 3D: Cone
(setq var-eq-surface-area-of-cone '(calcFunc-eq (var S var-S) (+ (* (var pi var-pi) (* (var r var-r) (var s var-s))) (* (var pi var-pi) (^ (var r var-r) 2)))))
(setq var-eq-lateral-surface-area-of-cone '(calcFunc-eq (var S var-S) (* (var pi var-pi) (* (var r var-r) (var s var-s)))))
(setq var-eq-volume-of-cone '(calcFunc-eq (var V var-V) (* (frac 1 3) (* (var pi var-pi) (* (^ (var r var-r) 2) (var h var-h))))))

;;; Geometry — 3D: Conical Frustum
(setq var-eq-surface-area-of-conical-frustrum '(calcFunc-eq (var S var-S) (+ (+ (* (var pi var-pi) (* (var s var-s) (+ (var r1 var-r1) (var r2 var-r2)))) (* (var pi var-pi) (^ (var r1 var-r1) (cplx 2 0)))) (* (var pi var-pi) (^ (var r2 var-r2) (cplx 2 0))))))
(setq var-eq-lateral-surface-area-of-conical-frustrum '(calcFunc-eq (var S var-S) (* (var pi var-pi) (* (var s var-s) (+ (var r1 var-r1) (var r2 var-r2))))))
(setq var-eq-volume-of-conical-frustrum '(calcFunc-eq (var V var-V) (* (frac 1 3) (* (var pi var-pi) (* (var h var-h) (+ (+ (^ (var R var-R) (cplx 2 0)) (^ (var r var-r) (cplx 2 0))) (* (var R var-R) (var r var-r))))))))

;;; Geometry — 3D: Pyramid
(setq var-eq-surface-area-of-pyramid '(calcFunc-eq (var S var-S) (+ (* (frac 1 2) (* (var p var-p) (var s var-s))) (var B var-B))))
(setq var-eq-lateral-surface-area-of-pyramid '(calcFunc-eq (var sl var-sl) (* (frac 1 2) (* (var p var-p) (var s var-s)))))
(setq var-eq-volume-of-pyramid '(calcFunc-eq (var V var-V) (* (frac 1 3) (* (var B var-B) (var h var-h)))))

;;; Trigonometry
(setq var-eq-pythagorean-trigonometric-identity '(calcFunc-eq (+ (^ (calcFunc-cos (var a var-a)) (cplx 2 0)) (^ (calcFunc-sin (var a var-a)) (cplx 2 0))) (cplx 1 0)))
(setq var-eq-law-of-sines '(calcFunc-eq (/ (var a var-a) (calcFunc-sin (var A var-A))) (/ (var b var-b) (calcFunc-sin (var B var-B))) (/ (var c var-c) (calcFunc-sin (var C var-C)))))
(setq var-eq-law-of-cosines-1 '(calcFunc-eq (^ (var c var-c) 2) (- (+ (^ (var a var-a) 2) (^ (var b var-b) 2)) (* 2 (* (var a var-a) (* (var b var-b) (calcFunc-cos (var C var-C))))))))
(setq var-eq-law-of-cosines-2 '(calcFunc-eq (calcFunc-cos (var C var-C)) (/ (- (+ (^ (var a var-a) 2) (^ (var b var-b) 2)) (^ (var c var-c) 2)) (* 2 (* (var a var-a) (var b var-b))))))

;;; Trig — Zeros and extrema
(setq var-eq-sine-roots '(calcFunc-eq (calcFunc-sin (* (var n var-n) (var pi var-pi))) (cplx 0 0)))
(setq var-eq-sine-maxima '(calcFunc-eq (calcFunc-sin (+ (* 2 (* (var n var-n) (var pi var-pi))) (/ (var pi var-pi) 2))) 1))
(setq var-eq-sine-minima '(calcFunc-eq (calcFunc-sin (- (* (cplx 2 0) (* (var n var-n) (var pi var-pi))) (/ (var pi var-pi) (cplx 2 0)))) (cplx -1 0)))
(setq var-eq-cosine-roots '(calcFunc-eq (calcFunc-cos (+ (* (var n var-n) (var pi var-pi)) (/ (var pi var-pi) 2))) 0))
(setq var-eq-cosine-maxima '(calcFunc-eq (calcFunc-cos (* 2 (* (var n var-n) (var pi var-pi)))) 1))
(setq var-eq-cosine-minima '(calcFunc-eq (calcFunc-cos (+ (* (cplx 2 0) (* (var n var-n) (var pi var-pi))) (var pi var-pi))) (cplx -1 0)))
(setq var-eq-tan-roots '(calcFunc-eq (calcFunc-tan (* (var n var-n) (var pi var-pi))) 0))
(setq var-eq-cotan-roots '(calcFunc-eq (calcFunc-cot (+ (* (var n var-n) (var pi var-pi)) (/ (var pi var-pi) 2))) 0))

;;; Trig — Asymptotes
(setq var-eq-tan-asymptotes '(+ (* (var n var-n) (var pi var-pi)) (/ (var pi var-pi) 2)))
(setq var-eq-cotan-asymptotes '(* (var n var-n) (var pi var-pi)))
(setq var-eq-secant-asymptotes '(+ (* (var n var-n) (var pi var-pi)) (/ (var pi var-pi) 2)))
(setq var-eq-cosecant-asymptotes '(* (var n var-n) (var pi var-pi)))

;;; Algebra
(setq var-eq-log-change-of-base-formula '(calcFunc-eq (calcFunc-log (var x var-x) (var b var-b)) (/ (calcFunc-log (var x var-x) (var k var-k)) (calcFunc-log (var b var-b) (var k var-k)))))
(setq var-eq-sum-of-cubes-identity '(calcFunc-eq (+ (^ (var a var-a) 3) (^ (var b var-b) 3)) (* (+ (var a var-a) (var b var-b)) (+ (- (^ (var a var-a) 2) (* (var a var-a) (var b var-b))) (^ (var b var-b) 2)))))
(setq var-eq-difference-of-cubes-identity '(calcFunc-eq (- (^ (var a var-a) 3) (^ (var b var-b) 3)) (* (- (var a var-a) (var b var-b)) (+ (+ (^ (var a var-a) 2) (* (var a var-a) (var b var-b))) (^ (var b var-b) 2)))))
(setq var-eq-standard-equation-of-reciprocal-function '(calcFunc-eq (var y var-y) (+ (/ (var a var-a) (- (var x var-x) (var h var-h))) (var k var-k))))
(setq var-eq-exponential-growth '(calcFunc-eq (calcFunc-f (var x var-x)) (* (var a var-a) (^ (var b var-b) (var x var-x)))))

;;; Conic sections — Parabola
(setq var-eq-vertex-form-of-vertical-parabola '(calcFunc-eq (^ (- (var x var-x) (var h var-h)) 2) (* 4 (* (var p var-p) (- (var y var-y) (var k var-k))))))
(setq var-eq-vertex-form-of-vertical-parabola-at-origin '(calcFunc-eq (^ (var x var-x) 2) (* 4 (* (var p var-p) (var y var-y)))))
(setq var-eq-standard-form-of-parabola '(calcFunc-eq (^ (- (var y var-y) (var k var-k)) 2) (* 4 (* (var p var-p) (- (var x var-x) (var h var-h))))))
(setq var-eq-standard-form-of-parabola-at-origin '(calcFunc-eq (^ (var y var-y) 2) (* 4 (* (var p var-p) (var x var-x)))))

;;; Finance
(setq var-eq-compound-interest '(calcFunc-eq (var A var-A) (* (var P var-P) (^ (+ (cplx 1 0) (/ (var r var-r) (var n var-n))) (* (var n var-n) (var t var-t))))))
(setq var-eq-continous-compound-interest '(calcFunc-eq (var A var-A) (* (var P var-P) (^ (var e var-e) (* (var r var-r) (var t var-t))))))
(setq var-eq-simple-compound-interest '(calcFunc-eq (var A var-A) (* (var P var-P) (^ (var r var-r) (var t var-t)))))

;;; BELOW: Added by calc.
;;; TODO: Assimilate these items.
