
;;; Mode settings stored by Calc on Tue Oct 14 11:25:47 2025
;; (setq calc-context-sensitive-enter t)
(setq calc-show-selections nil)
(setq calc-display-trail nil)
(setq calc-symbolic-mode t)
(setq calc-prefer-frac t)
;; (setq calc-vector-commas nil)
;;; End of mode settings

;;; Variable "var-eq-standard-equation-of-parabola" stored by Calc on Wed Dec 10 12:05:12 2025
(setq var-eq-standard-equation-of-parabola '(calcFunc-eq (^ (- (var x var-x) (var h var-h)) 2) (* 4 (* (var p var-p) (- (var y var-y) (var k var-k))))))

;;; Variable "var-eq-standard-equation-of-circle" stored by Calc on Fri Dec 12 16:36:29 2025
(setq var-eq-standard-equation-of-circle '(calcFunc-eq (+ (^ (- (var x var-x) (var h var-h)) 2) (^ (- (var y var-y) (var k var-k)) 2)) (^ (var r var-r) 2)))

;;; Variable "var-eq-law-of-cosines-1" stored by Calc on Sun Dec 14 13:47:42 2025
(setq var-eq-law-of-cosines-1 '(calcFunc-eq (^ (var c var-c) 2) (- (+ (^ (var a var-a) 2) (^ (var b var-b) 2)) (* 2 (* (var a var-a) (* (var b var-b) (calcFunc-cos (var C var-C))))))))

;;; Variable "var-eq-law-of-cosines-2" stored by Calc on Sun Dec 14 13:47:47 2025
(setq var-eq-law-of-cosines-2 '(calcFunc-eq (calcFunc-cos (var C var-C)) (/ (- (+ (^ (var a var-a) 2) (^ (var b var-b) 2)) (^ (var c var-c) 2)) (* 2 (* (var a var-a) (var b var-b))))))

;;; Variable "var-eq-pythagorean-trigonometric-identity" stored by Calc on Sun Dec 14 14:15:40 2025
(setq var-eq-pythagorean-trigonometric-identity '(calcFunc-eq (+ (^ (calcFunc-cos (var a var-a)) 2) (^ (calcFunc-sin (var a var-a)) 2)) 1))

;;; Variable "var-eq-lateral-surface-area-of-pyramid" stored by Calc on Sun Dec 14 14:46:46 2025
(setq var-eq-lateral-surface-area-of-pyramid '(calcFunc-eq (var sl var-sl) (/ (* (var s var-s) (var p var-p)) 2)))

;;; Variable "var-eq-area-of-circular-sector" stored by Calc on Sun Dec 14 14:59:45 2025
(setq var-eq-area-of-circular-sector '(calcFunc-eq (var A var-A) (/ (* (var a var-a) (^ (var r var-r) 2)) 2)))

;;; Variable "var-eq-general-area-of-triangle" stored by Calc on Sun Dec 14 15:07:09 2025
(setq var-eq-general-area-of-triangle '(calcFunc-eq (var A var-A) (/ (* (var a var-a) (* (var b var-b) (calcFunc-sin (var C var-C)))) 2)))

;;; Variable "var-eq-volume-of-pyramid" stored by Calc on Mon Dec 15 13:05:06 2025
(setq var-eq-volume-of-pyramid '(calcFunc-eq (var V var-V) (/ (* (var B var-B) (var h var-h)) 3)))

;;; Variable "var-eq-surface-area-of-conical-frustrum" stored by Calc on Tue Dec 16 11:31:37 2025
(setq var-eq-surface-area-of-conical-frustrum '(calcFunc-eq (var S var-S) (+ (+ (* (var pi var-pi) (* (var s var-s) (+ (var r1 var-r1) (var r2 var-r2)))) (* (var pi var-pi) (^ (var r1 var-r1) (cplx 2 0)))) (* (var pi var-pi) (^ (var r2 var-r2) (cplx 2 0))))))

;;; Variable "var-eq-lateral-surface-area-of-conical-frustrum" stored by Calc on Tue Dec 16 11:31:43 2025
(setq var-eq-lateral-surface-area-of-conical-frustrum '(calcFunc-eq (var S var-S) (* (var pi var-pi) (* (var s var-s) (+ (var r1 var-r1) (var r2 var-r2))))))

;;; Variable "var-eq-volume-of-conical-frustrum" stored by Calc on Tue Dec 16 12:36:50 2025
(setq var-eq-volume-of-conical-frustrum '(calcFunc-eq (var V var-V) (/ (* (var h var-h) (* (var pi var-pi) (+ (+ (^ (var R var-R) 2) (^ (var r var-r) 2)) (* (var R var-R) (var r var-r))))) 3)))
