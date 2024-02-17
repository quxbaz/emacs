;; Reusable function definitions


;; # Utils

(defun my/print-to-buffer (obj &optional buffer-name)
  "Prints OBJ to a buffer. If BUFFER-NAME is nil, print to *scratch* buffer."
  (let* ((buffer-name (or buffer-name "*scratch*"))
         (buffer (get-buffer buffer-name)))
    (princ obj buffer)
    (princ "\n" buffer)))

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

(defun my/line-text ()
  "Gets text string at the current line."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun my/region-text ()
  "Gets text within the region. If region is inactive, return nil."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun my/root-list-position (&optional max-depth)
  "Gets the position of the root list starting from point."
  (if (eq max-depth nil)
      (setq max-depth 100))
  (let ((current-point (point)))
    (condition-case nil
        (dotimes (n max-depth)
          (setq current-point (scan-lists current-point -1 1)))
      (scan-error nil))
    current-point))

(defun my/goto-root-list (&optional max-depth)
  "Moves point to opening parens of root list."
  (interactive)
  (if (eq max-depth nil)
      (setq max-depth 100))
  (condition-case nil
      (backward-up-list max-depth t t)
    (scan-error nil)
    (user-error nil)))


;; # Text navigation, selection

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
(defun my/mark-context ()
  "Marks the current context using the following logic:

    1. If region is inactive and point is before indentation, move to indentation
       and re-invoke.
    2. If region is active, and point is at end of line, and start of region is
       at indentation, mark entire line.
    3. If point is at end of line, mark line back to first textual char.
    4. If region is inactive and point is on (, mark sexp.
    5. If region is inactive and point is on ), mark sexp.
    6. If region is inactive, mark short word.
    7. If region is active and long word is already marked, mark sexp.
    8. If region is active and point is at (, mark parent sexp.
    9. If region is active and short word is already marked, mark long word.
   10. If region is active and short word is NOT marked, mark short word.

Typically, repeated invocations will go like this:

    short-word -> long-word -> sexp -> parent sexp"
  (interactive)
  (let ((origin (point))
        (short-word (current-word nil t))
        (long-word (current-word nil nil)))
    (cond
     ;; If point is before indentation, move to indentation and re-invoke.
     ((and (not (use-region-p))
           (< (current-column) (current-indentation)))
      (back-to-indentation)
      (call-interactively #'my/mark-context))
     ;; If region is active, and point is at end of line, and start of region is
     ;; at indentation, mark entire line.
     ((and (use-region-p)
           (= (point) (line-end-position))
           (= (region-beginning) (+ (line-beginning-position) (current-indentation)))
           (= (region-end) (line-end-position)))
      (beginning-of-line)
      (push-mark (point) nil t)
      (end-of-line))
     ;; If point is at end of line, mark line back to first textual char.
     ((= (point) (line-end-position))
      (beginning-of-line-text)
      (push-mark (point) nil t)
      (end-of-line))
     ;; If region is inactive and point is on (, mark sexp.
     ((and (not (use-region-p))
           (= (char-after) ?\())
      (push-mark (point) nil t)
      (forward-sexp)
      (exchange-point-and-mark))
     ;; If region is inactive and point is on ), mark sexp.
     ((and (not (use-region-p))
           (= (char-after) ?\)))
      (push-mark (+ (point) 1) nil t)
      (backward-up-list 1 t t))
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


;; # Search, replace, occur

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
          ;; Move point to avoid superfluous matching on current word.
          (if reverse?
              (goto-char (- (region-beginning) 1))
            (goto-char (region-end)))
          (funcall search-fn nil t)
          (isearch-yank-string text))
      (funcall search-fn))))

(defun my/isearch-forward-dwim () (interactive) (my/isearch-dwim))
(defun my/isearch-backward-dwim () (interactive) (my/isearch-dwim t))

(defun my/query-replace-dwim (&optional buffer?)
  "Performs an interactive search & replace. If region matches (current-word),
and point is at beginning of region, use region as the search string.

ARGUMENTS
buffer [optional] [bool] [default = nil]    If true, start replace at beginning of buffer."
  (interactive)
  (let ((short-word (current-word nil t))
        (long-word  (current-word nil nil)))
    (if (and (use-region-p)
             (= (point) (region-beginning))
             (or (string= (my/region-text) short-word)
                 (string= (my/region-text) long-word)))
        (let* ((text (my/region-text))
               (prompt (format "Query replace regexp (default %s  → [REGEX])" text))
               (regex (read-regexp prompt)))
          (deactivate-mark)
          (if buffer?
              (query-replace-regexp text regex nil 1 (buffer-size))
            (query-replace-regexp text regex)))
      (call-interactively #'query-replace-regexp))))

(defun my/query-replace-buffer-dwim ()
  "Performs an interactive search & replace from the beginning of the buffer.
If region matches (current-word), and point is at beginning of region, use
region as the search string."
  (interactive)
  (if (use-region-p)
      (my/query-replace-dwim t)
    (beginning-of-buffer)
    (call-interactively #'query-replace-regexp)))

(defun my/dired-to-emacs-d ()
  "Invokes a dired buffer to the user's emacs directory."
  (interactive)
  (kill-buffer (current-buffer))  ;; Kill current dired buffer.
  (dired user-emacs-directory))

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

(defun my/occur-dwim ()
  (interactive)
  (let ((short-word (current-word nil t))
        (long-word  (current-word nil nil)))
    (if (and (use-region-p)
             (or (string= (my/region-text) short-word)
                 (string= (my/region-text) long-word)))
        (let ((text (my/region-text)))
          (deactivate-mark)
          (occur text))
      (call-interactively 'occur))))


;; # Editing

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
    (let ((content (my/line-text))
          (col (current-column)))
      (end-of-line) (newline)
      (insert content)
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

(defun my/move-line-to-register (&optional register)
  "Moves line to point stored in register `@`"
  (interactive)
  (when (eq register nil)
    (setq register ?@))
  (save-excursion
    (kill-whole-line)
    (let ((buffer (buffer-name))
          (text (car kill-ring)))
      (jump-to-register register)
      (yank)
      ;; If register is in another buffer, switch back to current buffer.
      (if (not (string= (buffer-name) buffer))
          (switch-to-buffer buffer))
      (message (concat "Yanked: " (string-trim (string-chop-newline text)))))))

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

(defun my/comment-jsx (arg)
  (interactive "p")
  (let ((is-empty-line (my/is-line-empty??)))
    (save-excursion
      (if (use-region-p)
          (let ((start (region-beginning))
                (end (region-end)))
            (goto-char end)
            (if (not (my/is-line-empty??)) (insert " "))
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


;; # Other

(defun my/switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun my/config ()
  (interactive)
  (find-file (concat (file-name-as-directory user-emacs-directory) "my/")))

(defun my/flash-mode-line ()
  "Flash the mode line to communicate an effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


;; # Lisp, paredit

(defun my/mark-list ()
  (interactive)
  (if (/= (char-after) ?\()
      (backward-up-list 1 t t))
  (mark-sexp))

(defun my/eval-dwim ()
  "Evals either the current region, block, or line - in that order of preference."
  (interactive)
  (if (use-region-p)
      (eval-region (region-beginning) (region-end) t)
    (save-excursion
      (my/goto-root-list)
      ;; If the point is at an opening parens, eval the list.
      ;; Else-if point is to the right of a closing parens, eval the list.
      ;; Else eval the line.
      (cond ((looking-at "(")
             (eval-region (point) (scan-lists (point) 1 0) t))
            ((looking-back ")")
             (eval-last-sexp nil))
            (t
             (eval-region (point-at-bol) (point-at-eol) t)))))
  (my/flash-mode-line))

;; BUG: This does not work inside strings.
(defun my/eval-here ()
  "Evaluates the most immediate list at point."
  (interactive)
  (let ((start (scan-lists (point) -1 1))
        (end (scan-lists (point) 1 1)))
    (eval-region start end t)))

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

(defun my/kill-list (arg)
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


;; # org-mode

(defun my/org-table-mark-field ()
  (interactive)
  (if (not (looking-back "|[[:blank:]]"))
      (org-table-beginning-of-field 0))
  (set-mark-command nil)
  (org-table-end-of-field 0)
  (exchange-point-and-mark))


;; # org-table

;; Bind this as needed.
;; (local-set-key (kbd "M-w") 'my/org-table-save-field)
(defun my/org-table-save-field ()
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (save-excursion
      (my/org-table-mark-field)
      (kill-ring-save (region-beginning) (region-end)))
    (message "Saved: %s" (car kill-ring))))
