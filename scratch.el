;; Scratch buffer for testing out code.
;;
;;

(calc-load-everything)

(math-common-constant-factor
 '(+ (* 40 (^ (var x var-x) 2)) (* 20 (var x var-x))))

(math-factor-expr
 '(+ (* 40 (^ (var x var-x) 2)) (* 20 (var x var-x))))

(calc-encase-atoms 42)  ;; -> (cplex 42 0)
(calc-encase-atoms '(42 42 42))  ;; -> (42 (cplx 42 0) (cplx 42 0))
(math-format-flat-expr x 0)
(math-format-nice-expr x (frame-width))
(calc-rewrite-selection "MergeRules" many "merg")

;; -> 120 x^2 + 60 x + 30
(math-format-value '(+ (+ (* 120 (^ (var x var-x) 2)) (* 60 (var x var-x))) 30))

(with-current-buffer (calc-select-buffer)
  ;; (calc-replace-sub-formula (nth 1 calc-stack) expr product)
  (calc-wrapper
   (let ((entry '((+ (+ 42 42) (var x var-x)) 1 nil)))
     (calc-pop-push-record-list 1 "op" (car entry) 1 '((var x var-x))))))

(with-current-buffer (calc-select-buffer)
  (let ((entry (nth 1 calc-stack)))
    (calc-replace-sub-formula (car entry) (nth 2 entry) 42)))

(with-current-buffer (calc-select-buffer)
  ;; (calc-replace-sub-formula (nth 1 calc-stack) expr product)
  (calc-wrapper
   (let* ((entry (my/calc-first-active-entry))
          (sel (nth 2 entry)))
     (print sel)
     (calc-pop-push-record-list 1 "op" (car entry) 1 sel)
     ;; (calc-pop-push-record-list 1 "op" (car entry) 1 '(cplx 42))
     )))

(math-format-stack-value (cdr calc-stack))
(math-format-stack-value (cadr calc-stack))
(calc-locate-cursor-element (point))

(defun calc-sel-evaluate (arg)
  (interactive "p")
  (calc-slow-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	        (calc-sel-reselect calc-keep-selection)
	        (entry (calc-top num 'entry))
	        (sel (or (calc-auto-selection entry) (car entry))))
     (calc-with-default-simplification
      (let ((math-simplify-only nil))
	      (calc-modify-simplify-mode arg)
	      (let ((val (calc-encase-atoms (calc-normalize sel))))
	        (calc-pop-push-record-list 1 "jsmp"
				                             (list (calc-replace-sub-formula
					                                  (car entry) sel val))
				                             num
				                             (list (and calc-sel-reselect val))))))
     (calc-handle-whys))))

(let* ((entry (calc-top 1 'entry))
       (parent (calc-find-parent-formula (car entry) (nth 2 entry))))
  (let* ((new (calc-replace-sub-formula (car entry) (nth 2 entry) 99)))
    ;; (calc-pop-push-record-list 1 "edit" new)
    (calc-pop-push-record 1 "edit" new)))

(let* ((entry (calc-top 1 'entry))
       (parent (calc-find-parent-formula (car entry) (nth 2 entry))))
  (let* ((new (calc-replace-sub-formula (car entry) (nth 2 entry) 99)))
    (calc-wrapper
     (calc-pop-push-record-list 1 "edit" new))))

(let* ((entry (calc-top 1 'entry))
       (parent (calc-find-parent-formula (car entry) (nth 2 entry))))
  (let* ((new (calc-replace-sub-formula (car entry) (nth 2 entry) 99)))
    (calc-wrapper
     (calc-pop-push-record-list 1 "edit" 99))))

(calc-pop-push-record-lists)

(calc-replace-sub-formula (car entry)
							            parent
							            (delq (nth 2 entry)
								                (copy-sequence
								                 parent)))

(-> print upcase "foobar")

(calc-align-stack-window)

(defun calc-eval-num (n)
  (interactive "P")
  (calc-slow-wrapper
   (let* ((nn (prefix-numeric-value n))
	        (calc-internal-prec (cond ((>= nn 3) nn)
				                            ((< nn 0) (max (+ calc-internal-prec nn)
						                                       3))
				                            (t calc-internal-prec)))
	        (calc-symbolic-mode nil))
     (calc-with-default-simplification
      (calc-pop-push-record 1 "num" (math-evaluate-expr (calc-top 1)))))
   (calc-handle-whys)))
