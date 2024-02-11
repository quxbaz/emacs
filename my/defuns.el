;; Reusable function definitions


;; Utils

(defun my/is-line-empty? ()
  "Returns t if the line at point is empty, otherwise nil."
  (eq (point-at-bol) (point-at-eol)))

(defun my/string-at (pos &optional offset)
  "Gets the string at a specified point."
  (let ((offset (or offset 0)))
    (string (char-after (+ pos offset)))))

(defun my/string-at-point (&optional offset)
  "Gets the string at point."
  (let ((offset (or offset 0)))
    (string (char-after (+ (point) offset)))))

(defun my/region-text ()
  "Gets text within the region. If region is inactive, return nil."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    nil))


;; Text navigation, selection

(defun my/swap-points ()
  (interactive)
  (if (not (boundp 'next-point))
      (setq-local next-point 1))
  (setq-local prev-point (point))
  (goto-char next-point)
  (recenter)
  (setq-local next-point prev-point))

(defun my/match-paren ()
  "Move point to the matching parens. Falls back to moving to the parent parens."
  (interactive)
  ;; \s( and \s) represent the opening/closing delimiter character groups.
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (backward-up-list 1 t t))))

;; The Logic needed here is trickier than it would appear on the surface.
(defun my/mark-current-word ()
  "Marks the current word using the following logic:

    1. If region is inactive, mark short word.
    2. If region is active and long word is already marked, mark sexp.
    3. If region is active and point is at (, mark parent sexp.
    4. If region is active and short word is already marked, mark long word.
    5. If region is active and short word is NOT marked, mark short word.

Typically, repeated invocations will go like this:

    short-word -> long-word -> sexp -> parent sexp"
  (interactive)
  (let ((origin (point))
        (short-word (current-word nil t))
        (long-word (current-word nil nil)))
    (cond
     ;; If region is inactive, mark short word.
     ((or (and (not (use-region-p))
               (save-excursion
                 (backward-char (length short-word))
                 (search-forward short-word (+ origin (length short-word)) t))))
      (push-mark (match-end 0) nil t)
      (goto-char (match-beginning 0)))
     ;; If region is active and long word is already marked, mark sexp.
     ;; OR, if region is active and point is at (, mark parent sexp.
     ((and (use-region-p)
           (or (string= (my/region-text) long-word)
               (eq (char-after) ?\()))
      (backward-up-list 1 t t)
      (push-mark (point))
      (forward-list 1)
      (exchange-point-and-mark))
     ;; If region is active and short word is already marked, mark long word.
     ((and (use-region-p)
           (string= (my/region-text) short-word))
      (backward-char (length long-word))
      (search-forward long-word (+ origin (length long-word)) t)
      (push-mark (match-end 0) nil t)
      (goto-char (match-beginning 0)))
     ;; If region is active and short word is NOT marked, mark short word.
     ((and (use-region-p)
           (not (string= (my/region-text) short-word))
           (save-excursion
             (backward-char (length short-word))
             (search-forward short-word (+ origin (length short-word)) t)))
      (push-mark (match-end 0) nil t)
      (goto-char (match-beginning 0))))))

(defun my/mark-paragraph (arg)
  (interactive "p")
  (mark-paragraph arg t)
  (if (my/is-line-empty?)
      (forward-line)))

(defun my/outline-toggle-all ()
  (interactive)
  (if (not (bound-and-true-p outline-minor-mode))
      (outline-minor-mode t))
  (if (not (boundp 'show-headings-only))
      (setq-local show-headings-only nil))
  (if show-headings-only (outline-show-all) (outline-hide-body))
  (setq-local show-headings-only (not show-headings-only)))


;; Search, replace

(defun my/isearch-dwim (&optional reverse?)
  "Searches for a string. If region matches (current-word), search for that.

ARGUMENTS
REVERSE? [optional] [bool] [default = nil]    If true, search backwards."
  (interactive)
  (let ((search-fn (if reverse? #'isearch-backward-regexp #'isearch-forward-regexp))
        (short-word (current-word nil t))
        (long-word  (current-word nil nil)))
    (if (and (use-region-p)
             (or (string= (my/region-text) short-word)
                 (string= (my/region-text) long-word)))
        (let ((text (my/region-text)))
          (deactivate-mark)
          (funcall search-fn nil t)
          (isearch-yank-string text))
      (funcall search-fn))))

(defun my/isearch-forward-dwim () (interactive) (my/isearch-dwim))
(defun my/isearch-backward-dwim () (interactive) (my/isearch-dwim t))

(defun my/find-dired ()
  "Like find-dired, but takes a regex option and defaults to ignoring certain directories."
  (interactive)
  (let ((regex (read-from-minibuffer "find . -regex ")))
    (find-dired "."
                (concat "! -regex './node_modules/.*' "
                        "! -regex './.next/.*' "
                        (concat "-regex '" regex "'")))))

(defun my/find-string-dired ()
  "Finds an occurrence of a string. Like my/find-string-dired, but surrounds the regex with .*"
  (interactive)
  (let ((regex (read-from-minibuffer "find -regex '.*[REGEX].*'  |  [REGEX] = ")))
    (find-dired "."
                (concat "! -regex './node_modules/.*' "
                        "! -regex './.next/.*' "
                        (concat "-regex '.*" regex ".*'")))))

(defun my/find-jsx ()
  "Finds all js[x] files starting from the current directory."
  (interactive)
  (find-dired "." (concat "! -regex './node_modules/.*' "
                          "! -regex './.next/.*' "
                          "-regex './.*.jsx?'")))



;; Editing

(defun my/delete-char (arg)
  (interactive "p")
  (if (use-region-p)
      (delete-rectangle (region-beginning) (region-end))
    (delete-char arg)))

(defun my/kill-line (arg)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (if (= arg 1) (kill-line) (kill-line arg))))

(defun my/kill-block (arg)
  (interactive "p")
  (save-excursion
    (mark-paragraph arg)
    (kill-region (region-beginning) (region-end))))

(defun my/indent-block ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (indent-for-tab-command)))

(defun my/copy-line ()
  (interactive)
  (save-excursion
    (beginning-of-line-text)
    (kill-ring-save (point) (point-at-eol))
    (message "Copied current line.")))

(defun my/open-line ()
  "Opens a new line above and indents."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

(defun my/duplicate-line (arg)
  (interactive "p")
  (dotimes (n arg)
    (let ((col (current-column)))
      (copy-to-register '@ (point-at-bol) (point-at-eol))
      (end-of-line)
      (newline)
      (insert-register '@)
      (move-to-column col))))

(defun my/duplicate-block (arg)
  (interactive "p")
  (save-excursion
    (dotimes (n arg)
      (mark-paragraph)
      (copy-to-register '@ (region-beginning) (region-end))
      (insert-register '@))))

(defun my/transpose-line (&optional down?)
  "Moves a line up or down.

ARGUMENTS
DOWN? [bool] [default = t]    If true, transposes the line downwards."
  (interactive)
  (let ((pos (point))
        (col (current-column)))
    (if down?
        (progn
          (next-line)
          (transpose-lines 1)
          (previous-line))
      (transpose-lines 1)
      (previous-line 2))
    (move-to-column col)))

(defun my/comment-line ()
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

(defun my/comment-block (arg)
  (interactive "p")
  (if (use-region-p)
      (comment-dwim nil)
      (save-excursion
        (mark-paragraph arg)
        (comment-dwim nil))))

(defun my/is-current-line-empty? ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun my/comment-jsx (arg)
  (interactive "p")
  (let ((is-empty-line (my/is-current-line-empty?)))
    (save-excursion
      (if (use-region-p)
          (let ((start (region-beginning))
                (end (region-end)))
            (goto-char end)
            (if (not (my/is-current-line-empty?)) (insert " "))
            (insert "*/}")
            (goto-char start)
            (insert "{/* "))
        (beginning-of-line-text)
        (insert "{/* ")
        (end-of-line)
        (insert " */}")))
    (if (and is-empty-line (not (use-region-p)))
        (goto-char (+ (point) 4))))
  (message "** Comment JSX **"))

(defun my/uncomment-jsx (arg)
  (interactive "p")
  (if (use-region-p)
      (progn
        (save-excursion
          (let ((beginning (region-beginning))
                (end (region-end)))
            (replace-regexp "{/\\* " "" nil beginning end)
            (replace-regexp " \?\\*/}" "" nil beginning end))))
    (save-excursion
      (replace-regexp "{/\\* " "" nil (point-at-bol) (point-at-eol))
      (replace-regexp " \\*/}" "" nil (point-at-bol) (point-at-eol))))
  (message "** Uncomment JSX **"))

(defun my/toggle-jsx-comment (arg)
  (interactive "p")
  (let ((start-pos (if (use-region-p)
                       (region-beginning)
                     (save-excursion
                       (beginning-of-line-text) (point)))))
    (if (and (string= (my/string-at start-pos 0) "{")
             (string= (my/string-at start-pos 1) "/")
             (string= (my/string-at start-pos 2) "\*")
             (string= (my/string-at start-pos 3) " "))
        (my/uncomment-jsx arg)
      (my/comment-jsx arg))))

(defun my/close-html-tag ()
  (interactive)
  (my/duplicate-line 1)
  (back-to-indentation)
  (forward-char 1)
  (insert "/"))

(defun my/clear-buffer ()
  (interactive)
  (mark-whole-buffer)
  (kill-region (region-beginning) (region-end)))

(defun my/revert-buffer ()
  (interactive)
  (revert-buffer t t))


;; Other

(defun my/switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun my/config ()
  (interactive)
  (find-file "~/.emacs.d/my/"))

(defun my/flash-mode-line ()
  "Flash the mode line to communicate an effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


;; Lisp, paredit

(defun my/eval-dwim ()
  "Evals either the current region, block, or line - in that order."
  (interactive)
  (if (use-region-p)
      (eval-region (region-beginning) (region-end) t)
    (save-excursion
      ;; Move to top-most (root) parent sexp.
      (condition-case nil
          (dotimes (n 100) (paredit-backward-up))
        (scan-error nil))
      ;; IF the point is at an opening parens then eval that sexp.
      (if (eq (char-after) ?\()
          (progn
            (back-to-indentation) (push-mark (point) t nil) (forward-sexp)
            (eval-region (region-beginning) (point) t))
        ;; IF point is one character outside the sexp, eval the sexp.
        ;; ELSE eval the line.
        (if (eq (char-before) ?\))
            (eval-last-sexp nil)
          (eval-region (point-at-bol) (point-at-eol) t)))))
  (my/flash-mode-line))

(defun my/lisp-kill-ring-save-dwim ()
  (interactive)
  (if (use-region-p)
      (kill-ring-save nil nil t)
    (save-excursion
      (let ((n 0))
        (while (and (< n 20)
                    (not (eq (char-after) ?\()))
          (condition-case nil
              (backward-up-list 1 t)
            (scan-error (if (looking-back ")")
                            (backward-char))))
          (setq n (+ n 1))))
      (push-mark (point) t t)
      (forward-list 1 t)
      (kill-ring-save nil nil t)))
  (message (car kill-ring)))

(defun my/next-sexp ()
  (interactive)
  (if (eq (char-after) (string-to-char "("))
      (progn
        (paredit-forward)
        (paredit-forward)
        (paredit-backward))
    (condition-case nil
        (progn (backward-up-list) (my/next-sexp))
      (scan-error (progn (search-forward "(") (backward-char))))))

(defun my/prev-sexp ()
  (interactive)
  (if (eq (char-after) (string-to-char "("))
      (paredit-backward)
    (progn
      (condition-case nil
          (progn
            (paredit-forward-up)
            (paredit-backward)
            (my/prev-sexp))
        (scan-error (progn (search-backward ")") (my/match-paren)))))))

(defun my/kill-sexp (arg)
  (interactive "p")
  ;; Kill the sexp, not the parent sexp when point is on a (.
  (if (eq (char-after) ?\()
      (forward-char))
  (condition-case nil
      (progn (backward-up-list) (kill-sexp))
    (scan-error (my/kill-block arg))))

(defun my/open-new-round ()
  (interactive)
  (paredit-close-round-and-newline)
  (paredit-open-round))


;; org-mode

(defun my/org-table-mark-field ()
  (interactive)
  (if (not (looking-back "|[[:blank:]]"))
      (org-table-beginning-of-field 0))
  (set-mark-command nil)
  (org-table-end-of-field 0)
  (exchange-point-and-mark))


;; org-table

;; Bind this as needed.
;; (local-set-key (kbd "M-w") 'my/org-table-save-field)
(defun my/org-table-save-field ()
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (save-excursion
      (my/org-table-mark-field)
      (kill-ring-save (region-beginning) (region-end)))
    (message (format "Saved: %s" (car kill-ring)))))
