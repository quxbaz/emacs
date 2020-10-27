;; Reusable function definitions


;; Utils

(defun my-is-line-empty-p ()
  "Returns t if the line at point is empty, otherwise nil."
  (eq (point-at-bol) (point-at-eol)))


;; Text navigation, selection

(defun my-swap-points ()
  (interactive)
  (if (not (boundp 'next-point))
      (setq-local next-point 1))
  (setq-local prev-point (point))
  (goto-char next-point)
  (recenter)
  (setq-local next-point prev-point))

(defun my-match-paren ()
  "Move the point to the matching parenthesis."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

(defun my-mark-current-word ()
  "Selects the current word under the point."
  (interactive)
  (let* ((opoint (point))
         (word (current-word))
         (word-length (length word)))
    (if (save-excursion
          (backward-char word-length)
          (search-forward word (+ opoint (length word)) t))
        (progn (push-mark (match-end 0) nil t)
               (goto-char (match-beginning 0))))))

(defun my-mark-paragraph (arg)
  (interactive "p")
  (mark-paragraph arg t)
  (if (my-is-line-empty-p)
      (forward-line)))


;; Editing

(defun my-indent-block ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (indent-for-tab-command)))

(defun my-indent-region (n)
  (interactive)
  (if (use-region-p)
      (progn
        (indent-rigidly (region-beginning) (region-end) n))
    (my-indent-block)))

(defun my-copy-line ()
  (interactive)
  (save-excursion
    (beginning-of-line-text)
    (kill-ring-save (point) (point-at-eol))
    (message "Saved current line.")))

(defun my-open-line ()
  "Opens a new line above and indents."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

(defun my-duplicate-line (arg)
  (interactive "p")
  (dotimes (n arg)
    (let ((col (current-column)))
      (copy-to-register '@ (point-at-bol) (point-at-eol))
      (end-of-line)
      (newline)
      (insert-register '@)
      (move-to-column col))))

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

(defun my-comment-block ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (comment-dwim nil)))


;; Other

(defun my-switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun my-config ()
  (interactive)
  (find-file "~/.emacs.d/my/"))

(defun my-eval ()
  "Evals either the current line, defun, or region."
  (interactive)
  (if (use-region-p)
      (eval-region (region-beginning) (region-end) nil)
    (save-excursion
      (condition-case nil
          (dotimes (n 99) (paredit-backward-up))
        (scan-error nil))
      (if (string= (current-word) "defun")
          (eval-defun nil)
        (eval-region (point-at-bol) (point-at-eol) nil))))
  (my-flash-mode-line))

(defun my-flash-mode-line ()
  "Flash the mode line to communicate an effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


;; Lisp, paredit

(defun my-next-sexp ()
  (interactive)
  (if (eq (char-after) (string-to-char "("))
      (progn
        (paredit-forward)
        (paredit-forward)
        (paredit-backward))
    (progn
      (condition-case nil
          (progn (backward-up-list) (my-next-sexp))
        (scan-error (progn (search-forward "(") (backward-char)))))))

(defun my-prev-sexp ()
  (interactive)
  (if (eq (char-after) (string-to-char "("))
      (paredit-backward)
    (progn
      (condition-case nil
          (progn
            (paredit-forward-up)
            (paredit-backward)
            (my-prev-sexp))
        (scan-error (progn (search-backward ")") (my-match-paren)))))))

(defun my-kill-sexp ()
  (interactive)
  (backward-up-list)
  (paredit-kill))
