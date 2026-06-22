;; Reusable function definitions
;;
;; Interactive functions meant to be used as commands bound to a shortcut.


;; # Init

(defun my/init ()
  (interactive)
  (find-file-noselect (expand-file-name "my/*" user-emacs-directory) nil nil t)
  (dolist (filepath my/init-files)
    (if (file-exists-p filepath)
        (find-file-noselect filepath)
      (error "Init file does not exist: %s" filepath)))
  (switch-to-buffer "*Messages*"))


;; # Help

(defun my/help-dwim ()
  "Describes the symbol at point — function or variable (without confirmation)."
  (interactive)
  (let ((word (current-word nil nil)))
    (unless word (user-error "No symbol at point"))
    (describe-symbol (intern word))))

(defun my/goto-to-binding-definition ()
  "Jumps to a definition bound to a key sequence, in the other window."
  (interactive)
  (let ((key (read-key-sequence "Find command bound to key sequence:")))
    (if (and (stringp key) (string= key ""))
        (message "Quit")
      (find-function-on-key-other-window key)
      ;; find-function-on-key-other-window selects the other window at the
      ;; definition, so flash the defun there.
      (my/flash-region (save-excursion (beginning-of-defun) (point))
                       (save-excursion (end-of-defun) (point))))))


;; # Commands

(defun my/quit ()
  "Quits the minibuffer if it's open, otherwise do a normal keyboard-quit."
  (interactive)
  (if (active-minibuffer-window)
      (abort-recursive-edit)
    (keyboard-quit)))

(defun my/kill-terminal (&optional arg)
  "Like `save-buffers-kill-terminal', but with prefix ARG exit without the
\"really exit\" and active-process confirmations, prompting only to save
modified file-visiting buffers."
  (interactive "P")
  (if arg
      (let ((confirm-kill-emacs nil)
            (confirm-kill-processes nil))
        (save-buffers-kill-terminal))
    (save-buffers-kill-terminal)))


;; # Projects

(defun my/project-list-buffers ()
  (interactive)
  (project-list-buffers)
  (other-window -1))


;; # Buffers, windows, ibuffer

(defun my/switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun my/other-window ()
  (interactive)
  (other-window -1))

(defun my/split-window-right ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun my/ibuffer-delete-no-conf ()
  "Delete marked buffers if any are marked, otherwise delete buffer at point; no confirmation."
  (interactive)
  (let ((marked-buffers (ibuffer-get-marked-buffers)))
    (cond (marked-buffers (dolist (buffer marked-buffers)
                            (kill-buffer buffer))
                          (ibuffer-unmark-all nil))
          (t (when-let ((buffer (ibuffer-current-buffer)))
               (kill-buffer buffer)
               (next-line)))))
  (ibuffer-update nil t))



;; # Text navigation, selection

(defun my/match-delimiter ()
  "Moves point between delimiters."
  (interactive)
  ;; \s( and \s) represent the opening and closing delimiter character groups.
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ;; Match opening and closing quotes.
        ((and (looking-at "\"") (looking-back " ")) (forward-sexp) (backward-char 1))
        (t (backward-up-list 1 t t))))

(defun my/match-outside-delimiter ()
  "Moves point between the left of opening delimiter and the right of the
closing delimiter."
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
             ;; (kill-buffer previous-buffer)  ;; Uncomment to kill the existing buffer.
             )))
        (t (call-interactively 'dired-jump))))

(defun my/dired-up-directory ()
  "Like dired-up-directory, but doesn't spawn a new buffer."
  (interactive)
  (find-alternate-file ".."))

(defvar my/dired-goto-line-visit nil
  "When non-nil, `my/dired-ret' with a numeric prefix also
visits the file or directory at the target line.")

(defun my/dired-ret (&optional line)
  "Visit the file or directory at point, replacing the dired buffer.
With a numeric prefix LINE (type the digits, then RET — e.g. \"11 RET\"),
jump point to that line instead of visiting anything. If
`my/dired-goto-line-visit' is non-nil, also visit the file or directory
at the target line."
  (interactive "P")
  (if (not line)
      (dired-find-alternate-file)
    (goto-char (point-min))
    (forward-line (1- (prefix-numeric-value line)))
    (dired-move-to-filename)
    (when my/dired-goto-line-visit
      (dired-find-alternate-file))))

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


;; # dwim region commands

(defun my/key-spc ()
  "Inserts `SPC` normally. If region is active, deactivate region mode instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'keyboard-quit)
    (call-interactively 'self-insert-command)))

(defun my/key-comma ()
  "Inserts `,` normally. If region is active, call sort-lines on region instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'sort-lines)
    (call-interactively 'self-insert-command)))

(defun my/key-backslash ()
  "Inserts `\\` normally. If region is active, delete horizontal space instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'delete-whitespace-rectangle)
    (call-interactively 'self-insert-command)))

(defun my/key-pipe ()
  "Inserts `|` normally. If region is active, call align-regexp instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'align-regexp)
    (call-interactively 'self-insert-command)))

(defun my/key-i ()
  "Inserts `i` normally. If region is active, call string-rectangle instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'string-rectangle)
    (call-interactively 'self-insert-command)))

(defun my/key-k ()
  "Inserts `k` normally. If region is active, call kill-region instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-region)
    (call-interactively 'self-insert-command)))

(defun my/key-l ()
  "Inserts `l` normally. If region is active, call kill-rectangle instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'clear-rectangle)
    (call-interactively 'self-insert-command)))

(defun my/key-n ()
  "Inserts `n` normally. If region is active, call rectangle-number-lines instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'rectangle-number-lines)
    (call-interactively 'self-insert-command)))

(defun my/key-o ()
  "Inserts `o` normally. If region is active, call open-rectangle instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'open-rectangle)
    (call-interactively 'self-insert-command)))

(defun my/key-p ()
  "Inserts `p` normally. If region is active, call ispell instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'ispell)
    (call-interactively 'self-insert-command)))

(defun my/key-w ()
  "Inserts `w` normally. If region is active, call kill-ring-save instead."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (call-interactively 'self-insert-command)))

(defun my/key-x ()
  "Inserts `x` normally. If region is active, call exchange-point-and-mark instead."
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

(defun my/yank ()
  "Works just like yank, but also replaces the active region."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end)))
  (call-interactively 'yank))

(defun my/kill-block (arg)
  (interactive "p")
  (save-excursion
    (mark-paragraph arg)
    (kill-region (region-beginning) (region-end))))

(defun my/indent-block ()
  (interactive)
  (let ((indent-buffer (eq last-command 'my/indent-block)))
    (save-excursion
      (if indent-buffer (mark-whole-buffer) (mark-paragraph))
      (indent-for-tab-command)
      (if indent-buffer (message "Indenting buffer...done")))))

(defun my/open-line ()
  "Opens a new line above and indents."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

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
  (duplicate-dwim 1)
  (back-to-indentation)
  (forward-char 1)
  (insert "/"))

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
  (let* ((root-snippet-dir (car yas-snippet-dirs))
         (mode-snippet-dir (format "%s/%s/" root-snippet-dir (symbol-name major-mode))))
    (if (file-directory-p mode-snippet-dir)
        (dired mode-snippet-dir)
      (dired root-snippet-dir)
      (message "Could not find snippet directory: %s" mode-snippet-dir))))


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

(defun my/org-evaluate-time-range ()
  "Evaluates a time range, or a range since now."
  (interactive)
  (condition-case nil
      (call-interactively 'org-evaluate-time-range)
    (error (if (org-at-timestamp-p 'lax)
               (let* ((timestamp (match-string 0))
                      (days-ago (* (org-time-stamp-to-now timestamp) -1))
                      (postfix (if (or (= days-ago 0) (> (abs days-ago) 1)) "s" "")))
                 (message "%s day%s" days-ago postfix))))))

(defun my/org-open-at-point-dwim (arg)
  "Open link at point, or all links in region if mark is active."
  (interactive "P")
  (if (use-region-p)
      (my/org-open-links-in-region (region-beginning) (region-end))
    (org-open-at-point arg)))

(defun my/org-open-links-in-region (beg end)
  "Open all links in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (goto-char (org-element-property :begin link))
          (org-open-at-point))))))


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


;; # Dired

(defun my/dired-do-rename-here ()
  "Like dired-do-rename, but restricts target to the current window."
  (interactive)
  (let ((saved-dired-dwim-target dired-dwim-target))
    (setq dired-dwim-target nil)
    (unwind-protect
        (call-interactively 'dired-do-rename)
      (setq dired-dwim-target saved-dired-dwim-target))))

(defvar my/dired-load-handlers
  '(("lisp" . my/dired-load-slime))
  "Alist of (FILE-EXTENSION . LOADER) used by `my/dired-do-load'.
LOADER is called with the file name.  Files whose extension has no
entry are loaded as Emacs Lisp via `dired-load'.")

(defun my/dired-load-slime (file)
  "Load FILE into the connected Common Lisp image (e.g. stumpwm) via SLIME."
  (require 'slime)
  (slime-load-file file))

(defun my/dired-do-load (&optional arg)
  "Like dired-do-load, but dispatches on file type.
Loads the marked (or next ARG) files, choosing the loader by file
extension from `my/dired-load-handlers'.  Files with no matching
handler are loaded into Emacs as usual."
  (interactive "P")
  (require 'dired-aux)
  (dired-map-over-marks-check
   (lambda ()
     (let* ((file (dired-get-filename))
            (loader (cdr (assoc (file-name-extension file)
                                my/dired-load-handlers))))
       (if (not loader)
           (dired-load)
         (condition-case err
             (progn
               (funcall loader file)
               nil)
           (error
            (dired-log "Load error for %s:\n%s\n" file err)
            (dired-make-relative file))))))
   arg 'load t))

(defun my/find-name-dired ()
  "Like find-name-dired, but uses the current directory by default."
  (interactive)
  (let ((pattern (read-regexp "find . -name PATTERN: ")))
    (find-name-dired default-directory pattern)))

(defun my/dired-context-edit-file ()
  "Opens a program to edit marked files."
  (interactive)
  (let* ((paths (dired-get-marked-files))
         ;; Filter for image file types.
         (image-paths (seq-filter 'my/is-image-p paths))
         ;; Image paths joined and surrounded by single quotes.
         (cmd-arg (mapconcat (lambda (path) (format "'%s'" path)) image-paths " ")))
    (async-shell-command (format "gimp %s" cmd-arg))))

(defun my/dired-open-gallery ()
  "Opens a gallery program to view images in the current directory."
  (interactive)
  (async-shell-command (format "geeqie %s" (replace-regexp-in-string " " "\\\\ " default-directory))))

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
        ;; (shell-command (format "convert %s -resize 259x324 -quality 92 %s" path full-output-path))
        ;; (shell-command (format "convert %s -resize 518x648 -quality 92 %s" path full-output-path-2x))
        (shell-command (format "convert %s -resize 320x400 -quality 92 %s" path full-output-path))
        (shell-command (format "convert %s -resize 480x600 -quality 92 %s" path full-output-path-2x))
        ))
    (dired-unmark-all-marks)
    (goto-char marker)
    (revert-buffer)))



;; # Deadgrep

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


;; Magit

(defun my/git-staged-renames ()
  "Return a list of (OLD . NEW) conses for staged renames."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-prefix-p "R" line)
                    (pcase-let ((`(,_ ,old ,new) (split-string line "\t")))
                      (cons old new))))
                (magit-git-lines "diff" "--cached" "--find-renames"
                                 "--name-status"))))

(defun my/git-rename-for-file (file renames)
  "Return the (OLD . NEW) pair in RENAMES that involves FILE, or nil."
  (seq-find (lambda (r) (or (equal (car r) file) (equal (cdr r) file)))
            renames))

(defun my/magit-quick-commit ()
  "Stage and commit when exactly one file is modified.
Uses 'modified:   filename' as the commit message, or, when the staged
change is a rename, git's 'renamed:    old -> new' format."
  (interactive)
  (let* ((file-at-point (magit-file-at-point))
         (renames (my/git-staged-renames))
         (unstaged (magit-unstaged-files))
         (staged (magit-staged-files)))
    (cond
     (file-at-point
      ;; Highest priority: commit only the file under point.  A path-limited
      ;; commit ("git commit -- FILE") records just this file and leaves any
      ;; other staged files staged, so the staged state is preserved.
      (let ((rename (my/git-rename-for-file file-at-point renames)))
        (if rename
            ;; A detected rename is already fully staged (both sides); commit
            ;; the pair together so it records as a rename.
            (magit-run-git "commit" "-m"
                           (format "renamed:    %s -> %s" (car rename) (cdr rename))
                           "--" (car rename) (cdr rename))
          (magit-run-git "add" "--" file-at-point)
          (magit-run-git "commit" "-m" (format "modified:   %s" file-at-point)
                         "--" file-at-point))))
     (staged
      (let* ((file (car staged))
             (rename (my/git-rename-for-file file renames))
             (msg (if rename
                      (format "renamed:    %s -> %s" (car rename) (cdr rename))
                    (format "modified:   %s" file))))
        (magit-run-git "commit" "-m" msg)))
     (unstaged
      (let* ((file (car unstaged))
             (msg (format "modified:   %s" file)))
        (magit-run-git "add" file)
        (magit-run-git "commit" "-m" msg)))
     ((magit-untracked-files)
      (let* ((file (car (magit-untracked-files)))
             (msg (format "modified:   %s" file)))
        (magit-run-git "add" file)
        (magit-run-git "commit" "-m" msg)))
     (t
      (user-error "my/magit-quick-commit: no modified files")))))


;; php, web-mode

(defun my/eval-php ()
  "Evaluates the line or the region in php interpreter."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region (region-beginning) (region-end) "php -a")
    (shell-command-on-region (+ (line-beginning-position) (current-indentation))
                             (region-end)
                             "php -a")))
