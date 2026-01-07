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

(defun my/calc-pop ()
  "Pops an entry from the stack. Shows message if stack is empty."
  (interactive)
  (calc-wrapper
   (if (> (calc-stack-size) 0)
       (calc-pop 1)
     (deactivate-mark)
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
        (let* ((var-name (intern (char-to-string char)))
               (var-symbol (intern (concat "var-" (char-to-string char)))))
          (calc-wrapper
           (calc-push (list 'var var-name var-symbol))))
      (message "Invalid character. Must be a-z or A-Z."))))

(defun my/calc-evaluate (n)
  "Like calc-evaluate, but disables symbolic mode during evaluation."
  (interactive "p")
  (let ((calc-symbolic-mode nil))
    (calc-evaluate n)))

(defun my/calc-recall ()
  "Like calc-recall, but don't simplify."
  (interactive)
  (my/calc-dont-simplify
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
  (my/calc-apply-sel-or-top (expr replace-expr) ((prefix "fctr") (m 2))
    (my/calc-dont-simplify
     (let* ((factor (calc-top-n 1))
            (divided (-> (calcFunc-div expr factor) calcFunc-expand calcFunc-nrat calcFunc-expand math-simplify))
            (product (calcFunc-mul factor divided)))
       (calc-wrapper
        (replace-expr product)
        (calc-pop-stack 1))))))

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

(provide 'my/calc/stack)
