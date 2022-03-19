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

(defun my-outline-toggle-all ()
  (interactive)
  (if (not (bound-and-true-p outline-minor-mode))
      (outline-minor-mode t))
  (if (not (boundp 'show-headings-only))
      (setq-local show-headings-only nil))
  (if show-headings-only (outline-show-all) (outline-hide-body))
  (setq-local show-headings-only (not show-headings-only)))


;; Appearance, themes

(defun my-swap-theme-background ()
  (interactive)
  (let* ((background-1 "#282a36")
         (background-2 "#1c1e26")
         (current (face-attribute 'default :background))
         (next (if (equal current background-1) background-2 background-1)))
    (custom-set-faces `(default ((t (:background ,next)))))))


;; Editing

(defun my-delete-char (arg)
  (interactive "p")
  (if (use-region-p)
      (delete-rectangle (region-beginning) (region-end))
    (delete-char arg)))

(defun my-kill-line (arg)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (if (= arg 1) (kill-line) (kill-line arg))))

(defun my-kill-block (arg)
  (interactive "p")
  (save-excursion
    (mark-paragraph arg)
    (kill-region (region-beginning) (region-end))))

(defun my-indent-block ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (indent-for-tab-command)))

(defun my-copy-line ()
  (interactive)
  (save-excursion
    (beginning-of-line-text)
    (kill-ring-save (point) (point-at-eol))
    (message "Copied current line.")))

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

(defun my-duplicate-block (arg)
  (interactive "p")
  (save-excursion
    (dotimes (n arg)
      (mark-paragraph)
      (copy-to-register '@ (region-beginning) (region-end))
      (insert-register '@))))

(defun my-transpose-lines (arg)
  (interactive "p")
  (dotimes (n arg)
    (progn
      (let ((pos (point))
            (col (current-column)))
        (transpose-lines 1)
        (goto-char pos)
        (previous-line)
        (move-to-column col)))))

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

(defun my-comment-block (arg)
  (interactive "p")
  (if (use-region-p)
      (comment-dwim nil)
      (save-excursion
        (mark-paragraph arg)
        (comment-dwim nil))))

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun my-comment-jsx (arg)
  (interactive "p")
  (let ((is-empty-line (current-line-empty-p)))
    (save-excursion
      (if (use-region-p)
          (let ((start (region-beginning))
                (end (region-end)))
            (goto-char end)
            (if (not (current-line-empty-p)) (insert " "))
            (insert "*/}")
            (goto-char start)
            (insert "{/* "))
        (beginning-of-line-text)
        (insert "{/* ")
        (end-of-line)
        (insert " */}")))
    (if (and is-empty-line (not (use-region-p)))
        (goto-char (+ (point) 4)))))

(defun my-uncomment-jsx ()
  (interactive)
  (save-excursion
    (replace-regexp "{/\\* " "" nil (point-at-bol) (point-at-eol))
    (replace-regexp " \\*/}" "" nil (point-at-bol) (point-at-eol))))

(defun my-toggle-jsx-comment (arg)
  (interactive "p")
  (if (use-region-p)
      (my-comment-jsx arg)
    (save-excursion
      (beginning-of-line-text)
      (if (and (string= (string (char-after (+ (point) 0))) "{")
               (string= (string (char-after (+ (point) 1))) "/")
               (string= (string (char-after (+ (point) 2))) "\*")
               (string= (string (char-after (+ (point) 3))) " "))
          (my-uncomment-jsx)
        (my-comment-jsx arg)))))

(defun my-close-html-tag ()
  (interactive)
  (my-duplicate-line 1)
  (back-to-indentation)
  (forward-char 1)
  (insert "/"))

(defun my-clear-buffer ()
  (interactive)
  (mark-whole-buffer)
  (kill-region (region-beginning) (region-end)))

(defun my-revert-buffer ()
  (interactive)
  (revert-buffer t t))


;; Other

(defun my-dired ()
  "Opens dired in the current directory."
  (interactive)
  (dired default-directory))

(defun my-dired-other-window ()
  (interactive)
  (dired-other-window default-directory))

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
      (if (or (string= (current-word) "defun") (string= (current-word) "add-hook"))
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
