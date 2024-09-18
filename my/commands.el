;; Reusable function definitions
;;
;; Interactive functions meant to be used as commands bound to a shortcut.


;; # Help

(defun my/help-dwim ()
  "Describes the function at point (without confirmation)."
  (interactive)
  (describe-function (intern (current-word nil nil))))


;; # Buffers, windows

(defun my/switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun my/other-window ()
  (interactive)
  (other-window -1))


;; # Text navigation, selection

(defun my/match-delimiter ()
  "Moves point to the beginning of the matching delimiter."
  (interactive)
  ;; \s( and \s) represent the opening and closing delimiter character groups.
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ;; Match opening and closing quotes.
        ((and (looking-at "\"") (looking-back " ")) (forward-sexp) (backward-char 1))
        (t (backward-up-list 1 t t))))

(defun my/match-outside-delimiter ()
  "Moves point to the outside of the matching delimiter."
  (interactive)
  (cond
   ;; This needs to be the first condition.
   ((my/is-inside-string) (my/goto-beginning-of-string))
   ;; \s( and \s) represent the opening and closing delimiter character groups.
   ((looking-at "\\s\(") (forward-list 1))
   ((looking-back "\\s\)") (backward-list 1))
   ;; Match opening and closing quotes.
   ((looking-at "\"") (forward-sexp))
   ((looking-back "\"") (backward-sexp))
   (t (backward-up-list 1 t t))))


(defun my/mark-current-word (&optional extended-word)
  "Marks either the short or extended word around point."
  (interactive)
  (let* ((origin (point))
         (short-word (current-word nil t))
         (long-word (current-word nil nil))
         (word (if extended-word long-word short-word)))
    (save-excursion
      (condition-case nil
          (backward-char (length word))
        (error nil))
      (search-forward word (+ origin (length word)) t))
    (push-mark (match-end 0) nil t)
    (goto-char (match-beginning 0))))

(defun my/mark-context ()
  "Marks the current context in this order (cycles):
1. Current word (short word)
2. Current word (extended word)
3. Current string (if available)
4. Current list"
  (interactive)
  (when (not (eq last-command 'my/mark-context))
    (setq-local my/mark-context/origin (point))
    (setq-local my/mark-context/is-inside-string (my/is-inside-string))
    (setq-local my/mark-context/short-word (current-word nil t))
    (setq-local my/mark-context/long-word (current-word nil nil)))
  (cond ((and (my/is-at-opening-paren)
              (not (my/is-list-marked)))
         (mark-sexp))
        ((my/is-string-marked)
         (my/mark-list my/mark-context/origin))
        ((string= (my/region-text) my/mark-context/long-word)
         (cond (my/mark-context/is-inside-string
                (my/mark-string my/mark-context/origin))
               (t
                (my/mark-list my/mark-context/origin))))
        ((string= (my/region-text) my/mark-context/short-word)
         (goto-char my/mark-context/origin)
         (my/mark-current-word t))
        (t
         (goto-char my/mark-context/origin)
         (my/mark-current-word nil)
         (if (my/is-at-opening-paren)
             (deactivate-mark)))))

(defun my/mark-paragraph (arg)
  (interactive "p")
  (mark-paragraph arg t)
  (if (my/is-line-empty)
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

(defun my/isearch-dwim (&optional is-reverse)
  "Searches for a string. If region matches (current-word), search for that.

ARGUMENTS
IS-REVERSE [optional] [bool] [default = nil]    If true, search backwards."
  (interactive)
  (let ((search-fn (if is-reverse #'isearch-backward-regexp #'isearch-forward-regexp))
        (short-word (current-word nil t))
        (long-word  (current-word nil nil)))
    (if (and (use-region-p)
             (or (string= (my/region-text) short-word)
                 (string= (my/region-text) long-word)))
        (let ((text (my/region-text)))
          (deactivate-mark)
          ;; Move point to avoid superfluous matching on current word.
          (if is-reverse
              (goto-char (- (region-beginning) 1))
            (goto-char (region-end)))
          (funcall search-fn nil t)
          (isearch-yank-string text))
      (funcall search-fn))))

(defun my/isearch-forward-dwim () (interactive) (my/isearch-dwim))
(defun my/isearch-backward-dwim () (interactive) (my/isearch-dwim t))

(defun my/query-replace-dwim (&optional start-at-beginning)
  "Performs an interactive search & replace. If region matches (current-word),
and point is at beginning of region, use region as the search string.

ARGUMENTS
start-at-beginning [optional] [bool] [default = nil]    If true, start replace at beginning of buffer."
  (interactive)
  (let ((short-word (current-word nil t))
        (long-word  (current-word nil nil)))
    (if (and (use-region-p)
             (= (point) (region-beginning))
             (or (string= (my/region-text) short-word)
                 (string= (my/region-text) long-word)))
        (let* ((text (my/region-text))
               (prompt (format "Query replace regexp (default %s → [REGEX])" text))
               (regex (read-regexp prompt)))
          (deactivate-mark)
          (if start-at-beginning
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

(defun my/dired-jump ()
  "When in a non-dired buffer, jump to dired. If in dired, jump to Emacs directory."
  (interactive)
  (cond ((eq major-mode 'dired-mode)
         (unless (string= default-directory user-emacs-directory)
           (let ((previous-buffer (current-buffer)))
             (dired user-emacs-directory)
             (kill-buffer previous-buffer))))
        (t (call-interactively 'dired-jump))))

(defun my/dired-up-directory ()
  "Like dired-up-directory, but doesn't spawn a new buffer."
  (interactive)
  (find-alternate-file ".."))

(defun my/find-regex-dired ()
  "Like find-dired, but takes a regex option and defaults to ignoring certain directories."
  (interactive)
  (let ((regex (read-from-minibuffer "find . -regex ")))
    (find-dired "."
                (concat "! -regex './node_modules/.*' "
                        "! -regex './.next/.*' "
                        (concat "-regex '" regex "'")))))

(defun my/occur-dwim ()
  (interactive)
  (if (use-region-p)
      (progn
        (deactivate-mark)
        (occur (my/region-text)))
    (call-interactively 'occur)))

(defun my/find-command-definition ()
  "Finds a definition bound to a key sequence."
  (interactive)
  (let ((key (read-key-sequence "Find command bound to key sequence:")))
    (if (and (stringp key) (string= key ""))
        (message "Quit")
      (let ((command-name (symbol-name (key-binding key))))
        (xref-find-definitions command-name)
        (message command-name)))))


;; # dwim region commands

(defun my/key-spc ()
  "Inserts `SPC` normally. If region is active, deactivate region mode instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'keyboard-quit)
    (call-interactively 'self-insert-command)))

(defun my/key-k ()
  "Inserts `k` normally. If region is active, kill region instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-region)
    (call-interactively 'self-insert-command)))

(defun my/key-w ()
  "Inserts `w` normally. If region is active, save region to kill-ring instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (call-interactively 'self-insert-command)))

(defun my/key-x ()
  "Inserts `x` normally. If region is active, exchange point and mark instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'exchange-point-and-mark)
    (call-interactively 'self-insert-command)))


;; # Editing

(defun my/kill-ring-save-dwim ()
  "Saves the region if region is active, else save the current line."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (if (eq last-command 'my/kill-ring-save-dwim)
        (kill-ring-save (point-min) (point-max))
      (kill-ring-save (+ (line-beginning-position) (current-indentation))
                      (+ (line-end-position) 1))))
  (message "%s" (string-trim (car kill-ring))))

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

(defun my/open-line ()
  "Opens a new line above and indents."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

(defun my/duplicate-dwim (arg)
  (interactive "p")
  (if (use-region-p)
      (let ((lines (count-lines (region-beginning) (region-end))))
        (call-interactively #'duplicate-dwim)
        (deactivate-mark)
        (next-line lines))
    (dotimes (n arg)
      (duplicate-line)
      (next-line))))

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

(defun my/transpose-line-down ()
  (interactive)
  (my/transpose-line t))

(defun my/comment-line ()
  (interactive)
  (save-excursion
    (if (use-region-p)
        (progn
          (if (= (point) (region-beginning))
              (progn (beginning-of-line) (exchange-point-and-mark) (end-of-line))
            (progn (end-of-line) (exchange-point-and-mark) (beginning-of-line)))
          (comment-dwim nil))
      (beginning-of-line)
      (set-mark-command nil)
      (move-end-of-line nil)
      (comment-dwim nil))))

(defun my/comment-block (arg)
  (interactive "p")
  (if (use-region-p)
      (comment-dwim nil)
    (save-excursion
      (mark-paragraph arg)
      (comment-dwim nil))))

(defun my/close-html-tag ()
  (interactive)
  (my/duplicate-dwim 1)
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


;; # Math, numbers

(defun my/increment (&optional arg)
  "Increment next number on line by ARG."
  (interactive "P")
  (cond ((eq arg nil)
         (setq arg 1))
        ((equal arg '(4))
         (setq arg -1)))
  (let ((origin (point)))
    ;; Go to the starting position of the number.
    (while (looking-back "[0-9-\.]")
      (backward-char))
    ;; If number is found, increment it, and move point to start of number.
    (if (search-forward-regexp "-?[0-9]\+" (line-end-position) t)
        (progn
          (replace-match (number-to-string (+ (string-to-number (match-string 0)) arg)))
          (goto-char (match-beginning 0)))
      ;; If number is not found, return point to original position.
      (goto-char origin))))

(defun my/decrement (&optional arg)
  "Decrement next number on line by ARG."
  (interactive "P")
  (cond ((eq arg nil)
         (setq arg -1))
        ((equal arg '(4))
         (setq arg 1))
        (t (setq arg (* arg -1))))
  (my/increment arg))


;; # Keyboard Macros

(defun my/call-macro-dwim (arg)
  "Calls a kmacro. If region is active, apply kmacro to each line."
  (interactive "p")
  (if (use-region-p)
      (progn
        (apply-macro-to-region-lines (region-beginning) (region-end))
        (deactivate-mark))
    (kmacro-end-and-call-macro arg)))


;; # Mini-apps

(defun my/list-packages ()
  (interactive)
  (let ((existing-buffer (get-buffer "*Packages*")))
    (if existing-buffer
        (switch-to-buffer existing-buffer)
      (call-interactively 'list-packages))))

(defun my/visit-snippet-directory ()
  "Visits the yasnippet directory for the current major mode."
  (interactive)
  (let ((directory (file-name-as-directory (concat user-emacs-directory
                                                   "snippets/"
                                                   (symbol-name major-mode)))))
    (if (file-directory-p directory)
        (dired directory)
      (message "Could not find snippet directory for: %s" major-mode))))


;; # org-mode

(defun my/insert-uuid ()
  "Inserts a UUID at point generated from 'org-id-uuid."
  (interactive)
  (insert (org-id-uuid)))

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


;; # dired

(defun my/dired-edit-file ()
  "Opens a program to edit the current file."
  (interactive)
  (let* ((filename (dired-get-filename))
         (extension (file-name-extension filename)))
    (cond ((or (string= extension "jpg")
               (string= extension "jpeg")
               (string= extension "png"))
           (shell-command (format "gimp %s" filename)))
          (t (message "No program associated with file: %s" (file-name-nondirectory filename))))))

(defun my/dired-resize-image ()
  "Resize marked images and save to a subdirectory."
  (interactive)
  (let ((marker (point-marker)))
    (dolist (path (seq-filter 'my/is-image-p (dired-get-marked-files)))
      (let* ((filename (file-name-nondirectory path))                        ;; img.jpg
             (extension (file-name-extension filename))                      ;; jpg
             (no-extension (file-name-sans-extension filename))              ;; img
             (directory (file-name-directory path))                          ;; /path/to/
             (output-directory (file-name-as-directory no-extension))        ;; img/
             (full-output-path (concat directory output-directory filename)) ;; /path/to/img/img.jpg
             (full-output-path-2x                                            ;; /path/to/img/img@2x.jpg
              (format "%s%s%s@2x.%s" directory output-directory no-extension extension)))
        (if (not (file-directory-p output-directory))
            (dired-create-directory output-directory))
        (shell-command (format "convert %s -resize 320x400 -quality 92 %s" path full-output-path))
        (shell-command (format "convert %s -resize 480x600 -quality 92 %s" path full-output-path-2x))))
    (dired-unmark-all-marks)
    (goto-char marker)
    (revert-buffer)))

;; # deadgrep

(defun my/deadgrep-display-result ()
  "Displays the result in other window without moving point."
  (interactive)
  (deadgrep-visit-result-other-window)
  (other-window -1))

(defun my/deadgrep-display-next-result ()
  "Displays the next result in the other window."
  (interactive)
  (call-interactively 'deadgrep-forward-match)
  (deadgrep-visit-result-other-window)
  (other-window -1))

(defun my/deadgrep-display-prev-result ()
  "Displays the previous result in the other window."
  (interactive)
  (call-interactively 'deadgrep-backward-match)
  (deadgrep-visit-result-other-window)
  (other-window -1))
