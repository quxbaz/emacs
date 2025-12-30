;; -*- lexical-binding: t; -*-
;;
;; Core calc stack operations


(defun my/calc ()
  "Starts calc."
  (interactive)
  (cond ((string-match-p "^[*]" (buffer-name))
         (calc nil t t))
        ((= (count-windows) 1)
         (split-window-right)
         (other-window 1)
         (calc nil t t))
        (t
         (call-interactively 'calc))))

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

(defun my/calc-evaluate ()
  "Like calc-evaluate, but turns off symbolic mode during evaluation, then restores."
  (interactive)
  (let ((stored-symbolic-mode calc-symbolic-mode))
    (unwind-protect
        (progn (calc-symbolic-mode -1)
               (call-interactively 'calc-evaluate))
      (if stored-symbolic-mode
          (calc-symbolic-mode 1)
        (calc-symbolic-mode -1)))))

(defun my/calc-square ()
  "Squares a number."
  (interactive)
  (funcall (kmacro "I Q")))

(defun my/calc-factor-by ()
  "Factors an expression by an argument.
With no selection: factors stack level 2 by stack level 1.
With selection active: factors the selected sub-expression by the top of stack."
  (interactive)
  (my/calc-dont-simplify
   ;; Factor the selection (using the top stack entry as the factor).
   (if (my/calc-active-selection-p)
       (let* (;; Stack position of the selection. Use either the selection at the
              ;; current line, or the active selection closest to the stack top.
              (m (if (my/calc-active-selection-at-line-p)
                     (calc-locate-cursor-element (point))
                   (my/calc-first-active-entry-m)))
              ;; (formula, height [in lines], selection [or nil])
              (entry (nth m calc-stack))
              ;; The selection aka subformula.
              (sel (nth 2 entry))
              (factor (calc-top-n 1))
              (divided (math-simplify (calcFunc-expand (calcFunc-div sel factor))))
              ;; The replacement expression.
              (product (calcFunc-mul factor divided))
              ;; The replacement formula. From within the original formula (car
              ;; entry), replace the selection (sel) with the factored
              ;; expression (product).
              (new-formula (calc-replace-sub-formula (car entry) sel product)))
         (my/preserve-point
          (calc-wrapper
           ;; Push the new formula at the line position of the selection (m) and
           ;; reselect the new subformula (product).
           (calc-pop-push-record-list 1 "fctr" new-formula m product)
           (calc-pop-stack 1))))
     ;; No selection: Factor the second stack entry using the top stack entry
     ;; as the factor.
     (let* ((expr (calc-top-n 2))
            (factor (calc-top-n 1))
            (divided (math-simplify (calcFunc-expand (calcFunc-div expr factor))))
            (product (calcFunc-mul factor divided)))
       (calc-wrapper
        (calc-pop-push-record-list 2 "fctr" product))))))

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

(provide 'my/calc/stack)
