
;;; Mode settings stored by Calc on Wed Jan  7 16:06:18 2026
(setq calc-display-trail nil)
(setq calc-symbolic-mode t)
(setq calc-prefer-frac t)
(setq calc-always-load-extensions t)
;;; End of mode settings

;;; Variable "var-eq-standard-equation-of-parabola" stored by Calc on Wed Dec 10 12:05:12 2025
(setq var-eq-standard-equation-of-parabola '(calcFunc-eq (^ (- (var x var-x) (var h var-h)) 2) (* 4 (* (var p var-p) (- (var y var-y) (var k var-k))))))

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
