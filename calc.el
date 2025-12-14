
;;; Mode settings stored by Calc on Tue Oct 14 11:25:47 2025
;; (setq calc-context-sensitive-enter t)
(setq calc-show-selections nil)
(setq calc-display-trail nil)
(setq calc-symbolic-mode t)
(setq calc-prefer-frac t)
;; (setq calc-vector-commas nil)
;;; End of mode settings

;;; Variable "var-eq-of-parabola" stored by Calc on Wed Dec 10 12:05:12 2025
(setq var-eq-of-parabola '(calcFunc-eq (^ (- (var x var-x) (var h var-h)) 2) (* 4 (* (var p var-p) (- (var y var-y) (var k var-k))))))

;;; Variable "var-log-power-rule" stored by Calc on Thu Dec 11 23:07:40 2025
(setq var-log-power-rule '(calcFunc-assign (calcFunc-ln (^ (var a var-a) (var b var-b))) (* (var b var-b) (calcFunc-ln (var a var-a)))))

;;; Variable "var-eq-of-circle" stored by Calc on Fri Dec 12 16:36:29 2025
(setq var-eq-of-circle '(calcFunc-eq (+ (^ (- (var x var-x) (var h var-h)) 2) (^ (- (var y var-y) (var k var-k)) 2)) (^ (var r var-r) 2)))

;;; Variable "var-eq-law-of-cos-1" stored by Calc on Sun Dec 14 13:47:42 2025
(setq var-eq-law-of-cos-1 '(calcFunc-eq (^ (var c var-c) 2) (- (+ (^ (var a var-a) 2) (^ (var b var-b) 2)) (* 2 (* (var a var-a) (* (var b var-b) (calcFunc-cos (var C var-C))))))))

;;; Variable "var-eq-law-of-cos-2" stored by Calc on Sun Dec 14 13:47:47 2025
(setq var-eq-law-of-cos-2 '(calcFunc-eq (calcFunc-cos (var C var-C)) (/ (- (+ (^ (var a var-a) 2) (^ (var b var-b) 2)) (^ (var c var-c) 2)) (* 2 (* (var a var-a) (var b var-b))))))

;;; Variable "var-eq-py-trig-id" stored by Calc on Sun Dec 14 14:15:40 2025
(setq var-eq-py-trig-id '(calcFunc-eq (+ (^ (calcFunc-cos (var a var-a)) 2) (^ (calcFunc-sin (var a var-a)) 2)) 1))

;;; Variable "var-eq-pyramid-lat-surface-area" stored by Calc on Sun Dec 14 14:46:46 2025
(setq var-eq-pyramid-lat-surface-area '(calcFunc-eq (var sl var-sl) (/ (* (var l var-l) (var p var-p)) 2)))

;;; Variable "var-eq-area-of-circular-sector" stored by Calc on Sun Dec 14 14:59:45 2025
(setq var-eq-area-of-circular-sector '(calcFunc-eq (var A var-A) (/ (* (var a var-a) (^ (var r var-r) 2)) 2)))
