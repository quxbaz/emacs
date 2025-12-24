;; -*- lexical-binding: t; -*-
;;
;; Core calc stack operations


(defun my/calc ()
  "Starts calc."
  (interactive)
  (cond ((and (string= (buffer-name) "*scratch*")
              (= (count-windows) 1))
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
  (let ((saved-point (point)))
    (call-interactively 'calc-undo)
    (setf (point) saved-point)))

(defun my/calc-redo ()
  "Like calc-redo, but retains point position."
  (interactive)
  (let ((saved-point (point)))
    (call-interactively 'calc-redo)
    (setf (point) saved-point)))

(defun my/calc-kill ()
  "Like calc-kill, but retains point position."
  (interactive)
  (let ((saved-point (point)))
    (call-interactively 'calc-kill)
    (setf (point) saved-point)))

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
  (let ((pos (point)))
    (call-interactively 'calc-no-simplify-mode)
    (setf (point) pos)
    (my/flash-mode-line)))

(defun my/calc-toggle-big-language ()
  "Toggle between big language and normal display mode."
  (interactive)
  (calc-wrapper
   (if (eq calc-language 'big)
       (calc-normal-language)
     (calc-big-language))))

(provide 'my/calc/stack)
