;; Calc commands
;;
;;


;; Entry / Commands

(defun my/calc-edit ()
  "Opens edit mode or edits the current entry."
  (interactive)
  (let ((line (substring-no-properties (thing-at-point 'line))))
    (if (string-match "[0-9]+:" line)
        (funcall (kmacro "j`"))
      (funcall (kmacro "'`")))))

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

(defun my/calc-vector-edit ()
  "Begins a vector entry."
  (interactive)
  (if (minibufferp)
      (call-interactively 'self-insert-command)
    (funcall (kmacro "'`"))
    (insert "[]") (backward-char)))


;; Edit Mode

(defun my/calc-edit-history-prev ()
  "Recall previous calc history entry."
  (interactive)
  (when calc-alg-entry-history
    (let ((history-length (length calc-alg-entry-history)))
      (setq my/calc-history-index (min (1+ my/calc-history-index) (1- history-length)))
      (goto-char (point-min)) (forward-line 2) (delete-region (point) (point-max))
      (insert (nth my/calc-history-index calc-alg-entry-history)))))

(defun my/calc-edit-history-next ()
  "Recall next calc history entry."
  (interactive)
  (when calc-alg-entry-history
    (goto-char (point-min)) (forward-line 2) (delete-region (point) (point-max))
    (when (> my/calc-history-index 0)
      (setq my/calc-history-index (1- my/calc-history-index))
      (insert (nth my/calc-history-index calc-alg-entry-history)))))

(defun my/calc-edit-finish ()
  "Like calc-edit-finish, but pushes to calc-alg-entry-history."
  (interactive)
  (save-excursion
    (goto-char (point-min)) (forward-line 2) (beginning-of-line)
    (let ((text (string-trim (buffer-substring-no-properties (point) (point-max)))))
      (cond ((string= text "") nil)    ;; Don't save empty strings to history.
            ((string= text "[]") (delete-region (line-beginning-position) (1+ (line-end-position))))
            ((string= text (cadr calc-alg-entry-history)) nil)  ;; Don't save string to history if it's a duplicate of the previous entry.
            (t (push text calc-alg-entry-history)))))
  (calc-edit-finish))

(defun my/calc-edit-newline ()
  "Like newline, but also sets indentation."
  (interactive)
  (newline)
  (if (string= (string-trim (thing-at-point 'line)) "]")
      (delete-horizontal-space)))

(defun my/calc-toggle-brackets ()
  "Toggle between parentheses and square brackets at point."
  (interactive)
  (cond ((looking-at "(")
         (delete-char 1) (insert "[") (backward-char 1))
        ((looking-at ")")
         (delete-char 1) (insert "]") (backward-char 1))
        ((looking-at "\\[")
         (delete-char 1) (insert "(") (backward-char 1))
        ((looking-at "\\]")
         (delete-char 1) (insert ")") (backward-char 1))))


;; Expression Manipulation

(defun my/calc-commute ()
  "Like calc-sel-commute, but works on the line instead of the current selection."
  (interactive)
  (let ((saved-point (point)))
    (unwind-protect
        (progn
          (move-end-of-line nil)
          (call-interactively 'calc-sel-commute))
      (setf (point) saved-point))))

(defun my/calc-sel-jump-equals ()
  "Like calc-sel-jump-equals, but unselects after jumping."
  (interactive)
  (call-interactively 'calc-sel-jump-equals)
  (beginning-of-line)
  (search-forward-regexp "#")
  (backward-char)
  (call-interactively 'calc-unselect))
