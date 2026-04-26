
;;; Mode settings stored by Calc on Sat Apr 18 14:32:23 2026
(setq calc-symbolic-mode t)
(setq calc-prefer-frac t)
(setq calc-always-load-extensions t)
;;; End of mode settings

;;; Variable "var-eq-standard-equation-of-circle" stored by Calc on Fri Dec 12 16:36:29 2025
(setq var-eq-standard-equation-of-circle '(calcFunc-eq (+ (^ (- (var x var-x) (var h var-h)) 2) (^ (- (var y var-y) (var k var-k)) 2)) (^ (var r var-r) 2)))

;;; Variable "var-eq-law-of-cosines-1" stored by Calc on Sun Dec 14 13:47:42 2025
(setq var-eq-law-of-cosines-1 '(calcFunc-eq (^ (var c var-c) 2) (- (+ (^ (var a var-a) 2) (^ (var b var-b) 2)) (* 2 (* (var a var-a) (* (var b var-b) (calcFunc-cos (var C var-C))))))))

;;; Variable "var-eq-law-of-cosines-2" stored by Calc on Sun Dec 14 13:47:47 2025
(setq var-eq-law-of-cosines-2 '(calcFunc-eq (calcFunc-cos (var C var-C)) (/ (- (+ (^ (var a var-a) 2) (^ (var b var-b) 2)) (^ (var c var-c) 2)) (* 2 (* (var a var-a) (var b var-b))))))

;;; Variable "var-eq-pythagorean-trigonometric-identity" stored by Calc on Sun Jan  4 15:36:34 2026
(setq var-eq-pythagorean-trigonometric-identity '(calcFunc-eq (+ (^ (calcFunc-cos (var a var-a)) (cplx 2 0)) (^ (calcFunc-sin (var a var-a)) (cplx 2 0))) (cplx 1 0)))

;;; Variable "var-eq-lateral-surface-area-of-pyramid" stored by Calc on Sat Jan  3 15:49:40 2026
(setq var-eq-lateral-surface-area-of-pyramid '(calcFunc-eq (var sl var-sl) (* (frac 1 2) (* (var p var-p) (var s var-s)))))

;;; Variable "var-eq-area-of-circular-sector" stored by Calc on Mon Jan  5 14:01:56 2026
(setq var-eq-area-of-circular-sector '(calcFunc-eq (var A var-A) (* (frac 1 2) (* (^ (var r var-r) (cplx 2 0)) (var a var-a)))))

;;; Variable "var-eq-general-area-of-triangle" stored by Calc on Sat Jan  3 21:43:19 2026
(setq var-eq-general-area-of-triangle '(calcFunc-eq (var A var-A) (* (frac 1 2) (* (var a var-a) (* (var b var-b) (calcFunc-sin (var C var-C)))))))

;;; Variable "var-eq-volume-of-pyramid" stored by Calc on Sat Jan  3 21:42:21 2026
(setq var-eq-volume-of-pyramid '(calcFunc-eq (var V var-V) (* (frac 1 3) (* (var B var-B) (var h var-h)))))

;;; Variable "var-eq-surface-area-of-conical-frustrum" stored by Calc on Tue Dec 16 11:31:37 2025
(setq var-eq-surface-area-of-conical-frustrum '(calcFunc-eq (var S var-S) (+ (+ (* (var pi var-pi) (* (var s var-s) (+ (var r1 var-r1) (var r2 var-r2)))) (* (var pi var-pi) (^ (var r1 var-r1) (cplx 2 0)))) (* (var pi var-pi) (^ (var r2 var-r2) (cplx 2 0))))))

;;; Variable "var-eq-lateral-surface-area-of-conical-frustrum" stored by Calc on Tue Dec 16 11:31:43 2025
(setq var-eq-lateral-surface-area-of-conical-frustrum '(calcFunc-eq (var S var-S) (* (var pi var-pi) (* (var s var-s) (+ (var r1 var-r1) (var r2 var-r2))))))

;;; Variable "var-eq-volume-of-conical-frustrum" stored by Calc on Sat Jan  3 21:47:37 2026
(setq var-eq-volume-of-conical-frustrum '(calcFunc-eq (var V var-V) (* (frac 1 3) (* (var pi var-pi) (* (var h var-h) (+ (+ (^ (var R var-R) (cplx 2 0)) (^ (var r var-r) (cplx 2 0))) (* (var R var-R) (var r var-r))))))))

;;; Variable "var-eq-surface-area-of-sphere" stored by Calc on Wed Dec 17 14:56:06 2025
(setq var-eq-surface-area-of-sphere '(calcFunc-eq (var S var-S) (* 4 (* (var pi var-pi) (^ (var r var-r) 2)))))

;;; Variable "var-eq-volume-of-sphere" stored by Calc on Wed Dec 17 14:56:52 2025
(setq var-eq-volume-of-sphere '(calcFunc-eq (var V var-V) (* (frac 4 3) (* (var pi var-pi) (^ (var r var-r) 3)))))

;;; Variable "var-eq-volume-of-cylinder" stored by Calc on Thu Dec 18 22:06:41 2025
(setq var-eq-volume-of-cylinder '(calcFunc-eq (var V var-V) (* (var pi var-pi) (* (^ (var r var-r) (cplx 2 0)) (var h var-h)))))

;;; Variable "var-eq-volume-of-cone" stored by Calc on Sat Jan  3 21:41:47 2026
(setq var-eq-volume-of-cone '(calcFunc-eq (var V var-V) (* (frac 1 3) (* (var pi var-pi) (* (^ (var r var-r) 2) (var h var-h))))))

;;; Variable "var-eq-surface-area-of-cylinder" stored by Calc on Sat Jan  3 21:49:26 2026
(setq var-eq-surface-area-of-cylinder '(calcFunc-eq (var S var-S) (+ (* (cplx 2 0) (* (var pi var-pi) (* (var r var-r) (var h var-h)))) (* (cplx 2 0) (* (var pi var-pi) (^ (var r var-r) (cplx 2 0)))))))

;;; Variable "var-eq-lateral-surface-area-of-cylinder" stored by Calc on Sat Dec 20 20:26:20 2025
(setq var-eq-lateral-surface-area-of-cylinder '(calcFunc-eq (var S var-S) (* 2 (* (var h var-h) (* (var pi var-pi) (var r var-r))))))

;;; Variable "var-eq-sum-of-interior-polygon-angles" stored by Calc on Sat Jan  3 21:50:17 2026
(setq var-eq-sum-of-interior-polygon-angles '(calcFunc-eq (var S var-S) (* 180 (- (var n var-n) 2))))

;;; Variable "var-eq-lateral-surface-area-of-cone" stored by Calc on Sun Dec 21 15:14:49 2025
(setq var-eq-lateral-surface-area-of-cone '(calcFunc-eq (var S var-S) (* (var pi var-pi) (* (var r var-r) (var s var-s)))))

;;; Variable "var-eq-surface-area-of-cone" stored by Calc on Sun Dec 21 15:14:56 2025
(setq var-eq-surface-area-of-cone '(calcFunc-eq (var S var-S) (+ (* (var pi var-pi) (* (var r var-r) (var s var-s))) (* (var pi var-pi) (^ (var r var-r) 2)))))

;;; Variable "var-eq-standard-equation-of-reciprocal-function" stored by Calc on Sun Dec 21 22:25:35 2025
(setq var-eq-standard-equation-of-reciprocal-function '(calcFunc-eq (var y var-y) (+ (/ (var a var-a) (- (var x var-x) (var h var-h))) (var k var-k))))

;;; Variable "var-eq-surface-area-of-pyramid" stored by Calc on Sat Jan  3 21:52:18 2026
(setq var-eq-surface-area-of-pyramid '(calcFunc-eq (var S var-S) (+ (* (frac 1 2) (* (var p var-p) (var s var-s))) (var B var-B))))

;;; Variable "var-eq-area-of-trapezoid" stored by Calc on Sat Jan  3 21:53:52 2026
(setq var-eq-area-of-trapezoid '(calcFunc-eq (var A var-A) (* (frac 1 2) (* (var h var-h) (+ (var b1 var-b1) (var b2 var-b2))))))

;;; Variable "var-eq-area-of-triangle" stored by Calc on Wed Jan  7 14:52:57 2026
(setq var-eq-area-of-triangle '(calcFunc-eq (var A var-A) (* (frac 1 2) (* (var b var-b) (var h var-h)))))

;;; Variable "var-eq-compound-growth" stored by Calc on Sat Jan 10 01:33:18 2026
(setq var-eq-compound-growth '(calcFunc-eq (var A var-A) (* (var P var-P) (^ (+ (cplx 1 0) (/ (var r var-r) (var n var-n))) (* (var n var-n) (var t var-t))))))

;;; Variable "var-eq-continous-compound-growth" stored by Calc on Sat Jan 10 01:34:40 2026
(setq var-eq-continous-compound-growth '(calcFunc-eq (var A var-A) (* (var P var-P) (^ (var e var-e) (* (var r var-r) (var t var-t))))))

;;; Variable "var-eq-log-change-of-base-formula" stored by Calc on Mon Apr 13 02:20:20 2026
(setq var-eq-log-change-of-base-formula '(calcFunc-eq (calcFunc-log (var x var-x) (var b var-b)) (/ (calcFunc-log (var x var-x) (var k var-k)) (calcFunc-log (var b var-b) (var k var-k)))))

;;; Variable "var-eq-sum-of-cubes-identity" stored by Calc on Mon Apr 13 02:18:41 2026
(setq var-eq-sum-of-cubes-identity '(calcFunc-eq (+ (^ (var a var-a) 3) (^ (var b var-b) 3)) (* (+ (var a var-a) (var b var-b)) (+ (- (^ (var a var-a) 2) (* (var a var-a) (var b var-b))) (^ (var b var-b) 2)))))

;;; Variable "var-eq-difference-of-cubes-identity" stored by Calc on Mon Apr 13 02:19:08 2026
(setq var-eq-difference-of-cubes-identity '(calcFunc-eq (- (^ (var a var-a) 3) (^ (var b var-b) 3)) (* (- (var a var-a) (var b var-b)) (+ (+ (^ (var a var-a) 2) (* (var a var-a) (var b var-b))) (^ (var b var-b) 2)))))

;;; Variable "var-eq-exponential-growth-formula" stored by Calc on Mon Apr 13 22:32:59 2026
(setq var-eq-exponential-growth-formula '(calcFunc-eq (calcFunc-f (var x var-x)) (* (var a var-a) (^ (var b var-b) (var x var-x)))))

;;; Variable "var-eq-vertex-form-of-vertical-parabola" stored by Calc on Sun Apr 19 00:51:49 2026
(setq var-eq-vertex-form-of-vertical-parabola '(calcFunc-eq (^ (- (var x var-x) (var h var-h)) 2) (* 4 (* (var p var-p) (- (var y var-y) (var k var-k))))))

;;; Variable "var-eq-vertex-form-of-vertical-parabola-at-origin" stored by Calc on Sun Apr 19 00:53:02 2026
(setq var-eq-vertex-form-of-vertical-parabola-at-origin '(calcFunc-eq (^ (var x var-x) 2) (* 4 (* (var p var-p) (var y var-y)))))

;;; Variable "var-eq-standard-form-of-parabola-at-origin" stored by Calc on Sun Apr 19 00:54:46 2026
(setq var-eq-standard-form-of-parabola-at-origin '(calcFunc-eq (^ (var y var-y) 2) (* 4 (* (var p var-p) (var x var-x)))))

;;; Variable "var-eq-standard-form-of-parabola" stored by Calc on Sun Apr 19 17:36:30 2026
(setq var-eq-standard-form-of-parabola '(calcFunc-eq (^ (- (var y var-y) (var k var-k)) 2) (* 4 (* (var p var-p) (- (var x var-x) (var h var-h))))))

;;; Variable "var-eq-law-of-sines" stored by Calc on Tue Apr 21 16:35:05 2026
(setq var-eq-law-of-sines '(calcFunc-eq (/ (var a var-a) (calcFunc-sin (var A var-A))) (/ (var b var-b) (calcFunc-sin (var B var-B))) (/ (var c var-c) (calcFunc-sin (var C var-C)))))

;;; Variable "var-eq-simple-compound-growth" stored by Calc on Fri Apr 24 15:29:59 2026
(setq var-eq-simple-compound-growth '(calcFunc-eq (var A var-A) (* (var P var-P) (^ (var r var-r) (var t var-t)))))

;;; Variable "var-eq-cosine-roots" stored by Calc on Sun Apr 26 14:28:03 2026
(setq var-eq-cosine-roots '(calcFunc-eq (calcFunc-cos (+ (* (var n var-n) (var pi var-pi)) (/ (var pi var-pi) 2))) 0))

;;; Variable "var-eq-sine-roots" stored by Calc on Sun Apr 26 14:26:36 2026
(setq var-eq-sine-roots '(calcFunc-eq (calcFunc-sin (* (var n var-n) (var pi var-pi))) (cplx 0 0)))

;;; Variable "var-eq-tan-roots" stored by Calc on Sun Apr 26 16:45:51 2026
(setq var-eq-tan-roots '(calcFunc-eq (calcFunc-tan (* (var n var-n) (var pi var-pi))) 0))

;;; Variable "var-eq-cotan-roots" stored by Calc on Sun Apr 26 14:28:08 2026
(setq var-eq-cotan-roots '(calcFunc-eq (calcFunc-cot (+ (* (var n var-n) (var pi var-pi)) (/ (var pi var-pi) 2))) 0))

;;; Variable "var-eq-sine-maxima" stored by Calc on Sun Apr 26 14:57:26 2026
(setq var-eq-sine-maxima '(calcFunc-eq (calcFunc-sin (+ (* 2 (* (var n var-n) (var pi var-pi))) (/ (var pi var-pi) 2))) 1))

;;; Variable "var-eq-sine-minima" stored by Calc on Sun Apr 26 14:58:47 2026
(setq var-eq-sine-minima '(calcFunc-eq (calcFunc-sin (- (* (cplx 2 0) (* (var n var-n) (var pi var-pi))) (/ (var pi var-pi) (cplx 2 0)))) (cplx -1 0)))

;;; Variable "var-eq-cosine-maxima" stored by Calc on Sun Apr 26 15:02:34 2026
(setq var-eq-cosine-maxima '(calcFunc-eq (calcFunc-cos (* 2 (* (var n var-n) (var pi var-pi)))) 1))

;;; Variable "var-eq-cosine-minima" stored by Calc on Sun Apr 26 15:02:42 2026
(setq var-eq-cosine-minima '(calcFunc-eq (calcFunc-cos (+ (* (cplx 2 0) (* (var n var-n) (var pi var-pi))) (var pi var-pi))) (cplx -1 0)))
