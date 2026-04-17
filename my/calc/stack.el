;; -*- lexical-binding: t; -*-
;;
;; Core calc stack operations


(defun my/calc ()
  "Starts calc."
  (interactive)
  (cond ((or (string-match-p "^[*]" (buffer-name))
             (memq major-mode (list 'dired-mode 'magit-status-mode)))
         (calc nil t t))
        ((= (count-windows) 1)
         (split-window-right)
         (other-window 1)
         (calc nil t t))
        (t
         (call-interactively 'calc))))

(defun my/calc-no-simplify-mode ()
  "Like calc-no-simplify-mode, but retains point."
  (interactive)
  (my/preserve-point
   (call-interactively 'calc-no-simplify-mode)
   (my/flash-mode-line)))

(defun my/calc-toggle-big-language ()
  "Toggle between big language and normal display mode."
  (interactive)
  (calc-wrapper
   (if (eq calc-language 'big)
       (calc-normal-language)
     (calc-big-language))))

(defun my/calc-undo ()
  "Like calc-undo, but retains point position."
  (interactive)
  (my/preserve-point (call-interactively 'calc-undo)))

(defun my/calc-redo ()
  "Like calc-redo, but retains point position."
  (interactive)
  (my/preserve-point (call-interactively 'calc-redo)))

(defun my/calc-kill ()
  "Like calc-kill, but retains point position."
  (interactive)
  (my/preserve-point (call-interactively 'calc-kill)))

(defun my/calc-beginning-of-expression ()
  "Moves point to beginning of expression on current line."
  (interactive)
  (move-beginning-of-line nil)
  (when calc-line-numbering
    (condition-case nil
        (search-forward-regexp "^[0-9]+: *")
      (error nil))))

(defun my/calc-kill-ring-save-dwim ()
  "Saves the region if region is active, else save the current line."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (let* ((line (string-trim (substring-no-properties (thing-at-point 'line))))
           (no-prefix-line (replace-regexp-in-string "^[0-9]+:[[:space:]]*" "" line)))
      (kill-new no-prefix-line)))
  (message "%s" (string-trim (car kill-ring))))

(defun my/calc-roll-to-top ()
  "Moves the current entry to the top of the stack."
  (interactive)
  (calc-wrapper
   (let ((n (calc-locate-cursor-element (point))))
     (when (and n (> n 0))
       (calc-roll-up n)))))

(defun my/calc-roll-to-bottom ()
  "Moves the top stack item to the bottom of the stack."
  (interactive)
  (calc-wrapper
   (let ((n (calc-stack-size)))
     (when (> n 1)
       (calc-roll-down n)))))

(defun my/calc-pop ()
  "Delete subformula at point, or pop top of stack if cursor is at bottom.
Shows message if stack is empty."
  (interactive)
  (cond ((and (> (calc-stack-size) 0)
              (<= (calc-locate-cursor-element (point)) 0))
         (calc-pop 1))
        ((> (calc-stack-size) 0)
         (calc-del-selection))
        (t
         (deactivate-mark)
         (calc-align-stack-window)
         (message "Stack is empty."))))

(defun my/calc-ret ()
  "Duplicates the line. If selection is active, exit selection mode instead."
  (interactive)
  (if (my/calc-active-selection-p)
      (my/calc-clear-selections)
    (my/calc-duplicate)))

(defun my/calc-duplicate ()
  "Duplicates the nearest entry at point. If region is active, duplicate
just the region."
  (interactive)
  (cond ((use-region-p)
         (calc-wrapper
          (calc-push (math-read-expr (buffer-substring-no-properties (region-beginning) (region-end)))))
         (setf (point) (- (point-max) 2)))
        ((my/calc-at-stack-bottom-p)
         (call-interactively 'calc-enter))
        (t
         (let* ((line (string-trim (substring-no-properties (thing-at-point 'line))))
                (no-prefix-line (replace-regexp-in-string "^[0-9]+:[[:space:]]*" "" line)))
           (calc-wrapper
            (calc-push (math-read-expr no-prefix-line)))
           (setf (point) (- (point-max) 2))))))

(defun my/calc-duplicate-no-move ()
  "Like my/calc-duplicate, but doesn't move point."
  (interactive)
  (let ((n (calc-locate-cursor-element (point)))
        (col (current-column)))
    (call-interactively 'my/calc-duplicate)
    (when (and n (> n 0))
      (calc-cursor-stack-index (1+ n))
      (move-to-column col))))

(defun my/calc-duplicate-stack ()
  "Duplicates the entire calc stack."
  (interactive)
  (calc-wrapper
   (calc-push-list (reverse (mapcar #'car (cdr calc-stack))))))

(defun my/calc-store-stack ()
  "Stores the current calc stack in memory."
  (interactive)
  (setq my/calc-stored-stack (cdr calc-stack))
  (message "Stored calc stack.")
  (my/flash-mode-line))

(defun my/calc-recall-stack ()
  "Recalls the stored calc stack."
  (interactive)
  (if my/calc-stored-stack
      (progn
        (calc-wrapper
         (calc-push-list (reverse (mapcar #'car my/calc-stored-stack))))
        (message "Recalled calc stack."))
    (message "Stored stack not found.")))

(defun my/calc-quick-variable ()
  "Reads a character and pushes it as a variable onto the calc stack."
  (interactive)
  (let ((char (read-char-from-minibuffer "Enter a variable: ")))
    (if (or (and (>= char ?a) (<= char ?z))
            (and (>= char ?A) (<= char ?Z)))
        (my/preserve-point
         (let* ((var-name (intern (char-to-string char)))
                (var-symbol (intern (concat "var-" (char-to-string char)))))
           (calc-wrapper
            (calc-push (list 'var var-name var-symbol)))))
      (message "Invalid character. Must be a-z or A-Z."))))

(defun my/calc-evaluate (n)
  "Like calc-evaluate, but disables symbolic mode during evaluation."
  (interactive "p")
  (let ((calc-symbolic-mode nil))
    (calc-evaluate n)))

(defun my/calc-recall ()
  "Like calc-recall, but don't simplify."
  (interactive)
  (let ((calc-simplify-mode 'none))
    (call-interactively 'calc-recall)))

(defun my/calc-square ()
  "Squares a number."
  (interactive)
  (funcall (kmacro "I Q")))

(defun my/calc-factor-by ()
  "Factors an expression by an argument.
With no selection: factors stack level 2 by stack level 1.
With selection active: factors the selected expression by the top of stack."
  (interactive)
  (my/calc-apply-sel-or-top (expr replace-expr) ((m 2) (prefix "fctr"))
    (let* ((calc-simplify-mode 'none)
           (factor (calc-top-n 1))
           (divided (-> (calcFunc-div expr factor) calcFunc-expand calcFunc-nrat calcFunc-expand math-simplify))
           (factored (calcFunc-mul factor divided)))
      (calc-wrapper
       (replace-expr factored)
       (calc-pop-stack 1)))))

(defun my/math-ref-angle (x)
  "Given an angle, gets its reference angle."
  (let ((result (calcFunc-mod x 360)))
    (cond ((= (calcFunc-geq result 270) 1) (calcFunc-sub 360 result))
          ((= (calcFunc-geq result 180) 1) (calcFunc-sub result 180))
          ((= (calcFunc-geq result 90) 1) (calcFunc-sub 180 result))
          (t result))))

(defun my/calc-ref-angle (arg)
  "Given an angle, gets its reference angle."
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "refa" 'my/math-ref-angle arg)))

(defun my/calc-vector-flatten ()
  "Flattens a vector."
  (interactive)
  (calc-wrapper
   (let ((flat-vector (list 'calcFunc-arrange (calc-top-n 1) (prefix-numeric-value 0))))
     (calc-enter-result 1 "flat" flat-vector))))

(defun my/calc-cath ()
  "Compute a leg of a right triangle given the hypotenuse and the other leg.
If stack has >= 2 items: pops leg (level 1) and hypotenuse (level 2), returns √(h²-a²).
If stack has 1 item: pops leg (level 1), assumes hypotenuse = 1 (unit circle), returns √(1-a²)."
  (interactive)
  (save-excursion
    (calc-wrapper
     (if (= (calc-stack-size) 1)
         (let* ((leg (calc-top-n 1))
                (result (calcFunc-sqrt (calcFunc-sub 1 (calcFunc-pow leg 2)))))
           (calc-enter-result 1 "cath" result))
       (let* ((leg (calc-top-n 1))
              (hyp (calc-top-n 2))
              (result (calcFunc-sqrt (calcFunc-sub (calcFunc-pow hyp 2) (calcFunc-pow leg 2)))))
         (calc-enter-result 2 "cath" result))))))

(defun my/calc-substitute (oldname)
  "Like calc-substitute, but the new-name prompt starts empty."
  (interactive "sSubstitute old: ")
  (calc-slow-wrapper
   (let (old new (num 1) expr)
     (if (or (equal oldname "") (equal oldname "$") (null oldname))
         (setq new (calc-top-n 1)
               old (calc-top-n 2)
               expr (calc-top-n 3)
               num 3)
       (let ((newname (read-string (concat "Substitute old: " oldname ", new: "))))
         (if (or (equal newname "") (equal newname "$") (null newname))
             (setq new (calc-top-n 1)
                   expr (calc-top-n 2)
                   num 2)
           (setq new (math-read-expr newname))
           (if (eq (car-safe new) 'error)
               (error "Bad format in expression: %s" (nth 1 new)))
           (setq expr (calc-top-n 1)))
         (setq old (math-read-expr oldname))
         (if (eq (car-safe old) 'error)
             (error "Bad format in expression: %s" (nth 1 old)))
         (or (math-expr-contains expr old)
             (error "No occurrences found"))))
     (calc-enter-result num "sbst" (math-expr-subst expr old new)))))


(defun my/calc-trail-yank-at-point ()
  "Push the trail entry at point onto the calc stack and switch to calc."
  (interactive)
  (beginning-of-line)
  (when (or (looking-at "Emacs Calc")
            (looking-at "----")
            (looking-at " ? ? ?[^ \n]* *$")
            (looking-at "..?.?$"))
    (error "Can't yank that line"))
  (when (looking-at ".*, \\.\\.\\., ")
    (error "Can't yank (vector was abbreviated)"))
  (forward-char 4)
  (search-forward " ")
  (let* ((next (save-excursion (forward-line 1) (point)))
         (str  (buffer-substring (point) (1- next)))
         (val  (math-read-plain-expr str)))
    (if (eq (car-safe val) 'error)
        (error "Can't yank that line: %s" (nth 2 val))
      (calc-wrapper
       (calc-enter-result 0 "yank" val)))))


(defun my/calc-auto-solve--sorted-vars (expr)
  "Return unique non-constant variables in EXPR, sorted alphabetically."
  (let (vars)
    (cl-labels ((collect (e)
                  (cond ((and (eq (car-safe e) 'var) (not (math-const-var e)))
                         (cl-pushnew e vars :test #'equal))
                        ((listp e) (mapc #'collect (cdr e))))))
      (collect expr))
    (sort vars (lambda (a b) (string< (symbol-name (nth 1 a))
                                      (symbol-name (nth 1 b)))))))

(defun my/calc-auto-solve--solved-for (expr)
  "Return the variable EXPR is solved for (plain var alone on one side), or nil."
  (when (eq (car-safe expr) 'calcFunc-eq)
    (let ((lhs (nth 1 expr))
          (rhs (nth 2 expr)))
      (cond ((eq (car-safe lhs) 'var) lhs)
            ((eq (car-safe rhs) 'var) rhs)))))

(defun my/calc-auto-solve ()
  "Solve top-of-stack for a variable.
With one variable, solves for it. With multiple, solves for the first
alphabetically, or cycles to the next if already solved for one."
  (interactive)
  (let* ((expr (calc-top-n 1))
         (vars (my/calc-auto-solve--sorted-vars expr))
         (n    (length vars)))
    (when (> n 0)
      (let* ((solved-for (and (> n 1) (my/calc-auto-solve--solved-for expr)))
             (var (if solved-for
                      (let* ((idx  (cl-position solved-for vars :test #'equal))
                             (next (mod (1+ (or idx -1)) n)))
                        (nth next vars))
                    (car vars)))
             (result (math-solve-eqn expr var nil)))
        (if result
            (calc-wrapper
             (calc-enter-result 1 "slv" result))
          (message "Can't solve for %s" (math-format-flat-expr var 0)))))))


(provide 'my/calc/stack)
