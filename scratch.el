;; Scratch buffer for testing out code.
;;
;;

(calc-encase-atoms 42)  ;; -> (cplex 42 0)
(calc-encase-atoms '(42 42 42))  ;; -> (42 (cplx 42 0) (cplx 42 0))
(math-format-flat-expr x 0)
(math-format-nice-expr x (frame-width))

(with-current-buffer (calc-select-buffer)
  (math-format-stack-value (nth 1 calc-stack)))

(with-current-buffer (calc-select-buffer)
  (insert (calc-top)))

(with-current-buffer (calc-select-buffer)
  (pp calc-stack)
  nil)

(with-current-buffer (calc-select-buffer)
  ;; (calc-replace-sub-formula (nth 1 calc-stack) expr product)
  (calc-wrapper
   (let ((entry '((+ (+ 42 42) (var x var-x)) 1 nil)))
     (calc-pop-push-record-list 1 "op" (car entry) 1 '((var x var-x))))))

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
calc-edit-top

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

(defun calc-push-list (vals &optional m sels)
  (while vals
    (if calc-executing-macro
	      (calc-push-list-in-macro vals m sels)
      (save-excursion
	      (calc-select-buffer)
	      (let* ((val (car vals))
	             (entry (list val 1 (car sels)))
	             (mm (+ (or m 1) calc-stack-top)))
	        (calc-cursor-stack-index (1- (or m 1)))
	        (if (> mm 1)
	            (setcdr (nthcdr (- mm 2) calc-stack)
		                  (cons entry (nthcdr (1- mm) calc-stack)))
	          (setq calc-stack (cons entry calc-stack)))
	        (let ((buffer-read-only nil))
	          (insert (math-format-stack-value entry) "\n"))
	        (calc-record-undo (list 'push mm))
	        (calc-set-command-flag 'renum-stack))))
    (setq vals (cdr vals)
	        sels (cdr sels))))

;; My edits
(defun calc-push-list (vals &optional m sels)
  ;; Loop through vals until list is empty.
  (while vals
    (save-excursion

      ;; Switch context to the calc buffer.
	    (calc-select-buffer)

	    (let* ((val (car vals))
	           (entry (list val 1 (car sels)))
             ;; mm has something to do with with the insertion position of the new entry.
	           (mm (+ (or m 1) calc-stack-top)))

	      (calc-cursor-stack-index (1- (or m 1)))

        ;; Enter the new stack entry either at the top of the stack or in the middle somewhere.
	      (if (> mm 1)
	          (setcdr (nthcdr (- mm 2) calc-stack)
		                (cons entry (nthcdr (1- mm) calc-stack)))
	        (setq calc-stack (cons entry calc-stack)))

        ;; Insert a visual linebreak after the new entry.
	      (let ((buffer-read-only nil))
	        (insert (math-format-stack-value entry) "\n"))

        ;; Allow operation to be undoable.
	      (calc-record-undo (list 'push mm))

        ;; Renumber the stack.
	      (calc-set-command-flag 'renum-stack)))

    ;; Loop to the next item.
    (setq vals (cdr vals)
	        sels (cdr sels))))

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

(defun calc-refresh (&optional align)
  (interactive)
  (and (derived-mode-p 'calc-mode)
       (not calc-executing-macro)
       (let* ((inhibit-read-only t)
	            (save-point (point))
	            (save-mark (ignore-errors (mark)))
	            (save-aligned (looking-at "\\.$"))
	            (thing calc-stack)
	            (calc-any-evaltos nil))
	       (setq calc-any-selections nil)
	       (erase-buffer)
         (when calc-show-banner
           (calc--header-line  "Emacs Calculator Mode" "Emacs Calc"
                               (* 2 (/ (window-width) 3)) -3))
	       (while thing
	         (goto-char (point-min))
	         (insert (math-format-stack-value (car thing)) "\n")
	         (setq thing (cdr thing)))
	       (calc-renumber-stack)
	       (if calc-display-dirty
	           (calc-wrapper (setq calc-display-dirty nil)))
	       (and calc-any-evaltos calc-auto-recompute
	            (calc-wrapper (calc-refresh-evaltos)))
	       (if (or align save-aligned)
	           (calc-align-stack-window)
	         (goto-char save-point))
	       (if save-mark (set-mark save-mark))))
  (and calc-embedded-info (not (derived-mode-p 'calc-mode))
       (with-current-buffer (aref calc-embedded-info 1)
	       (calc-refresh align)))
  (setq calc-refresh-count (1+ calc-refresh-count)))
