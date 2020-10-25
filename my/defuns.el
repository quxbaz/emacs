;; Reusable function definitions


;; Editing

(defun my-indent-region (n)
  (interactive)
  (indent-rigidly (region-beginning) (region-end) n))

(defun my-open-line ()
  "Opens a new line above and indents."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

(defun my-duplicate-line ()
  (interactive)
  (let ((col (current-column)))
    (copy-to-register '@ (line-beginning-position) (line-end-position))
    (end-of-line)
    (newline)
    (insert-register '@)
    (move-to-column col)))

(defun my-comment-line ()
  (interactive)
  (save-excursion
    (if (use-region-p)
        (progn
          (if (= (point) (region-beginning))
              (progn (beginning-of-line) (exchange-point-and-mark) (end-of-line))
            (progn (end-of-line) (exchange-point-and-mark) (beginning-of-line)))
          (comment-dwim nil))
      (progn
        (beginning-of-line)
        (set-mark-command nil)
        (move-end-of-line nil)
        (comment-dwim nil)))))

(defun my-match-paren ()
  "Move the cursor to the matching parenthesis."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

;; ::TODO::
(defun my-mark-current-word ()
  "Selects the current word."
  (interactive)
  (let* ((opoint (point))
         (word (current-word))
         (word-length (length word)))
    (if (save-excursion
          ;; Avoid signaling error when moving beyond buffer.
          (if (> (point-min)  (- (point) word-length))
              (beginning-of-buffer)
            (forward-char (- (length word))))
          (search-forward word (+ opoint (length word))
                          'noerror))
        (progn (push-mark (match-end 0) nil t)
               (goto-char (match-beginning 0)))
      (error "No word at point" word))))


;; Other

(defun my-switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun my-edit-config ()
  (find-file "~/.emacs.d/init.el"))

(defun my-eval ()
  "Evals either a defun or region depending if the region is active."
  (interactive)
  (if (use-region-p)
      (eval-region (region-beginning) (region-end) nil)
    (eval-defun nil))
  (my-flash-mode-line))

(defun my-flash-mode-line ()
  "Flash the mode line to communicate an effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


;; paredit, lisp

(defun my-kill-sexp ()
  (interactive)
  (backward-up-list)
  (paredit-kill))
