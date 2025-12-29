;; Scratch buffer for testing out code.
;;
;;

(math-format-stack-value (cdr calc-stack))
(math-format-stack-value (cadr calc-stack))
(calc-locate-cursor-element (point))

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

(defun my/calc-factor-by ()
  "Factors an expression by an argument.
With no selection: factors stack level 2 by stack level 1.
With selection active: factors the selected sub-expression by the top of stack."
  (interactive)
  (my/calc-dont-simplify
   (if (my/calc-active-selection-at-cursor-p)
       ;; Factor the selection by top of stack.
       (let* ((m (calc-locate-cursor-element (point)))
              (stack (nth m calc-stack))
              (expr (nth 2 stack))
              (factor (calc-top-n 1))
              (divided (math-simplify (calcFunc-expand (calcFunc-div expr factor))))
              (product (calcFunc-mul factor divided))
              (replacement-expr (calc-replace-sub-formula expr expr product)))
         (calc-wrapper
          (calc-pop-push-record-list 1 "fctr" replacement-expr m)
          (calc-pop-stack 1)))
     ;; No selection: factor top two stack items.
     (let* ((expr (calc-top-n 2))
            (factor (calc-top-n 1))
            (divided (math-simplify (calcFunc-expand (calcFunc-div expr factor))))
            (product (calcFunc-mul factor divided)))
       (calc-wrapper
        (calc-pop-push-record-list 2 "fctr" product))))))

(defun my/calc-factor-by ()
  "Factors the stack by an argument."
  (interactive)
  (my/calc-dont-simplify
   (let* ((a (calc-top-n 2))
          (b (calc-top-n 1))
          (divided (math-simplify (calcFunc-expand (calcFunc-div a b))))
          (product (calcFunc-mul b divided)))
     (calc-wrapper
      (calc-pop-stack 2)
      (calc-push product)))))

(calc-pop-push-record-lists)

(calc-replace-sub-formula (car entry)
							            parent
							            (delq (nth 2 entry)
								                (copy-sequence
								                 parent)))

(defun calc-finish-selection-edit (num sel reselect)
  (let ((buf (current-buffer))
	      (str (buffer-substring calc-edit-top (point-max)))
	      (start (point)))
    (switch-to-buffer calc-original-buffer)
    (let ((val (math-read-expr str)))
      (if (eq (car-safe val) 'error)
	        (progn
	          (switch-to-buffer buf)
	          (goto-char (+ start (nth 1 val)))
	          (error (nth 2 val))))
      (calc-wrapper
       (calc-preserve-point)
       (if calc-edit-disp-trail
	         (calc-trail-display 1 t))
       (setq val (calc-encase-atoms (calc-normalize val)))
       (let ((expr (calc-top num 'full)))
	       (if (calc-find-sub-formula expr sel)
	           (calc-pop-push-record-list 1 "edit"
					                              (list (calc-replace-sub-formula
					                                     expr sel val))
					                              num
					                              (list (and reselect val)))
	         (calc-push val)
	         (error "Original selection has been lost")))))))

(defun calc-delete-selection (n)
  (let ((entry (calc-top n 'entry)))
    (if (nth 2 entry)
	      (if (eq (nth 2 entry) (car entry))
	          (progn
	            (calc-pop-stack 1 n t)
	            (calc-push-list '(0) n))
	        (let ((parent (calc-find-parent-formula (car entry) (nth 2 entry)))
		            (repl nil))
	          (calc-preserve-point)
	          (calc-pop-stack 1 n t)
	          (cond ((or (memq (car parent) '(* / %))
		                   (and (eq (car parent) '^)
			                      (eq (nth 2 parent) (nth 2 entry))))
		               (setq repl 1))
		              ((memq (car parent) '(vec calcFunc-min calcFunc-max)))
		              ((and (assq (car parent) calc-tweak-eqn-table)
			                  (= (length parent) 3))
		               (setq repl 'del))
		              (t
		               (setq repl 0)))
	          (cond
	           ((eq repl 'del)
	            (calc-push-list (list
			                         (calc-normalize
				                        (calc-replace-sub-formula
				                         (car entry)
				                         parent
				                         (if (eq (nth 2 entry) (nth 1 parent))
				                             (nth 2 parent)
				                           (nth 1 parent)))))
			                        n))
	           (repl
	            (calc-push-list (list
			                         (calc-normalize
				                        (calc-replace-sub-formula (car entry)
							                                            (nth 2 entry)
							                                            repl)))
			                        n))
	           (t
	            (calc-push-list (list
			                         (calc-normalize
				                        (calc-replace-sub-formula (car entry)
							                                            parent
							                                            (delq (nth 2 entry)
								                                                (copy-sequence
								                                                 parent)))))
			                        n)))))
      (calc-pop-stack 1 n t))))
