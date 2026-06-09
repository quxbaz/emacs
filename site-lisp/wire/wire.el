;;; wire.el --- Send annotated regions to a running Claude instance  -*- lexical-binding: t; -*-

;; Author: David
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.0

;;; Commentary:

;; Wire sends the active region --- or, with no region, a message about
;; the whole file/buffer --- plus a short annotation to a *running*
;; Claude Code session living in a kitty terminal window.
;;
;; The message it builds carries a context header: the project root and
;; git branch, the file (or, for fileless buffers, the buffer name and
;; major mode), and, when a region is active, its line range and the
;; region text fenced for the buffer's language.  It is injected into
;; Claude's prompt via kitty's remote control protocol
;; (`kitty @ send-text') and auto-submitted.
;;
;; Why kitty: a process cannot write to another terminal's stdin on a
;; modern kernel (TIOCSTI is disabled, and a PTY's master fd is held by
;; the terminal emulator).  kitty owns that fd and exposes a write
;; command, so it is the transport.
;;
;; One-time setup --- in ~/.config/kitty/kitty.conf:
;;
;;     allow_remote_control yes
;;     listen_on unix:/tmp/kitty
;;
;; then restart kitty.  Emacs is a separate process, so every remote
;; command is sent with `--to <socket>'.
;;
;; Usage:
;;
;;   M-x wire-mode               ; enable in a buffer (or global-wire-mode)
;;   C-c y s                     ; pick which Claude window to target
;;   mark a region (or not), C-c y y
;;   edit the pre-filled message, C-c C-c or C-RET   ; send (C-c C-k cancels)
;;   C-c y v                     ; focus the target's kitty window
;;
;; Default keys under `wire-mode', prefix C-c y: y (dispatch), s
;; (select-target), l (list instances), v (visit target).
;;
;; See QUICKSTART.md for a fuller walk-through.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'project nil t)

;;;; Customization

(defgroup wire nil
  "Send annotated regions to a running Claude instance via kitty."
  :group 'tools
  :prefix "wire-")

(defcustom wire-kitty-program "kitty"
  "Name of (or path to) the kitty executable."
  :type 'string)

(defcustom wire-kitty-socket "unix:/tmp/kitty"
  "Base kitty remote-control socket, matching `listen_on' in kitty.conf.
kitty appends `-PID' to this path for each instance, so wire expands
the base with a `-*' glob to find the real socket(s); an exact match
is also honored.  Set this to the value you put in `listen_on'
\(without any PID)."
  :type 'string)

(defcustom wire-process-regexp "claude"
  "Regexp matched against a kitty window's foreground command line.
Windows whose foreground process matches are treated as Claude
instances and offered as dispatch targets."
  :type 'regexp)

(defcustom wire-bracketed-paste "enable"
  "Value for kitty `send-text --bracketed-paste'.
With \"enable\", the multi-line message body is wrapped in
bracketed-paste markers so embedded newlines do not submit the
prompt early; a separate carriage return submits it."
  :type '(choice (const "enable") (const "auto") (const "disable")))

;;;; State

(defvar wire-target nil
  "Currently selected Claude target, or nil.
A plist of the form (:id ID :socket SOCKET :label LABEL), where ID is
the kitty window id, SOCKET is the `unix:PATH' that owns it, and LABEL
is a human description.  Session-only.")

;;;; kitty remote control

(defun wire--socket-paths ()
  "Resolve `wire-kitty-socket' to a list of concrete `unix:PATH' addresses.
kitty appends `-PID' to the configured `listen_on' path, so the base
path is expanded with a `-*' glob; an exact match is also honored."
  (let* ((base (string-remove-prefix "unix:" wire-kitty-socket))
         (paths '()))
    (when (file-exists-p base)
      (push base paths))
    (dolist (p (file-expand-wildcards (concat base "-*")))
      (when (file-exists-p p)
        (push p paths)))
    (mapcar (lambda (p) (concat "unix:" p))
            (delete-dups (nreverse paths)))))

(defun wire--kitty (socket input &rest args)
  "Run a kitty remote command against SOCKET with ARGS, returning (EXIT . OUTPUT).
ARGS are appended after `@ --to SOCKET'.  If INPUT is non-nil it is
fed on stdin.  OUTPUT captures both stdout and stderr."
  (let ((out (generate-new-buffer " *wire-kitty*"))
        (full (append (list "@" "--to" socket) args)))
    (unwind-protect
        (let ((exit
               (if input
                   (with-temp-buffer
                     (insert input)
                     (apply #'call-process-region (point-min) (point-max)
                            wire-kitty-program nil out nil full))
                 (apply #'call-process wire-kitty-program nil out nil full))))
          (cons exit (with-current-buffer out (buffer-string))))
      (kill-buffer out))))

(defun wire--parse-json (string)
  "Parse STRING as JSON into alists and lists."
  (if (fboundp 'json-parse-string)
      (json-parse-string string :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (json-read-from-string string))))

(defun wire--ls ()
  "Return kitty window plists (:id :title :cwd :cmdline :socket).
Queries every discovered socket and aggregates their windows.  Signal
a `user-error' if no socket is reachable."
  (let ((sockets (wire--socket-paths))
        (windows '())
        (errors '()))
    (unless sockets
      (user-error
       "wire: no kitty socket found matching %s-*. Enable remote control in kitty.conf and restart kitty"
       (string-remove-prefix "unix:" wire-kitty-socket)))
    (dolist (sock sockets)
      (pcase-let ((`(,exit . ,output) (wire--kitty sock nil "ls")))
        (if (and (integerp exit) (zerop exit))
            (dolist (osw (wire--parse-json output))
              (dolist (tab (alist-get 'tabs osw))
                (dolist (win (alist-get 'windows tab))
                  (let* ((fg (car (alist-get 'foreground_processes win)))
                         (cmdline (string-join (alist-get 'cmdline fg) " ")))
                    (push (list :id (alist-get 'id win)
                                :title (alist-get 'title win)
                                :cwd (or (alist-get 'cwd fg) (alist-get 'cwd win))
                                :cmdline cmdline
                                :socket sock)
                          windows)))))
          (push (cons sock (string-trim output)) errors))))
    (when (and (null windows) errors)
      (user-error "wire: kitty remote control failed: %s" (cdr (car errors))))
    (nreverse windows)))

(defun wire--claude-windows ()
  "Return the subset of `wire--ls' running a Claude process."
  (seq-filter (lambda (w)
                (string-match-p wire-process-regexp (plist-get w :cmdline)))
              (wire--ls)))

(defun wire--label (w)
  "Human label for window plist W."
  (format "%d  %s  [%s]%s"
          (plist-get w :id)
          (abbreviate-file-name (or (plist-get w :cwd) "?"))
          (or (plist-get w :title) "")
          (let ((sock (plist-get w :socket)))
            (if sock
                (format "  {%s}"
                        (file-name-nondirectory
                         (string-remove-prefix "unix:" sock)))
              ""))))

;;;; Target selection

;;;###autoload
(defun wire-select-target ()
  "Pick a running Claude instance as the dispatch target."
  (interactive)
  (let ((windows (wire--claude-windows)))
    (unless windows
      (user-error "wire: no running Claude instances found (regexp %S)"
                  wire-process-regexp))
    (let* ((table (mapcar (lambda (w) (cons (wire--label w) w)) windows))
           (choice (if (= (length table) 1)
                       (caar table)
                     (completing-read "Claude target: " table nil t)))
           (w (cdr (assoc choice table))))
      (setq wire-target (list :id (plist-get w :id)
                              :socket (plist-get w :socket)
                              :title (plist-get w :title)
                              :label choice))
      (message "wire: target set to %s" choice)
      wire-target)))

;;;###autoload
(defun wire-list-instances ()
  "Echo the running Claude instances kitty can see."
  (interactive)
  (let ((windows (wire--claude-windows)))
    (if windows
        (message "wire: %d Claude instance(s):\n%s"
                 (length windows)
                 (mapconcat #'wire--label windows "\n"))
      (message "wire: no running Claude instances found"))))

;;;###autoload
(defun wire-doctor ()
  "Diagnose the wire/kitty setup and report results in a buffer."
  (interactive)
  (let ((lines '())
        (ok t))
    (cl-flet ((add (fmt &rest args) (push (apply #'format fmt args) lines)))
      ;; 1. kitty executable.
      (let ((exe (executable-find wire-kitty-program)))
        (if exe
            (add "[ok]   kitty executable: %s" exe)
          (setq ok nil)
          (add "[FAIL] kitty executable %S not found on PATH" wire-kitty-program)))
      ;; 2. Socket discovery + reachability.
      (add "[..]   socket base: %s" wire-kitty-socket)
      (condition-case err
          (let ((sockets (wire--socket-paths)))
            (if (null sockets)
                (progn
                  (setq ok nil)
                  (add "[FAIL] no socket matching %s-* (kitty appends -PID)"
                       (string-remove-prefix "unix:" wire-kitty-socket))
                  (add "       -> in kitty.conf set:  allow_remote_control yes")
                  (add "                              listen_on %s" wire-kitty-socket)
                  (add "       then FULLY restart kitty"))
              (add "[ok]   %d socket(s) discovered:" (length sockets))
              (dolist (sock sockets)
                (pcase-let ((`(,exit . ,out) (wire--kitty sock nil "ls")))
                  (if (and (integerp exit) (zerop exit))
                      (add "         %s  (reachable)" sock)
                    (setq ok nil)
                    (add "         %s  [FAIL: %s]" sock (string-trim out)))))))
        (error
         (setq ok nil)
         (add "[FAIL] could not run kitty: %s" (error-message-string err))))
      ;; 3. Claude instances (only if remote control worked).
      (when ok
        (let ((windows (ignore-errors (wire--claude-windows))))
          (if windows
              (progn
                (add "[ok]   %d Claude instance(s) (regexp %S):"
                     (length windows) wire-process-regexp)
                (dolist (w windows) (add "         %s" (wire--label w))))
            (add "[warn] no windows match %S -- is Claude running in kitty?"
                 wire-process-regexp))))
      ;; 4. Current target.
      (add "[info] current target: %s"
           (if wire-target (plist-get wire-target :label) "none selected")))
    (let ((buf (get-buffer-create "*wire-doctor*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "wire-doctor\n===========\n\n"
                  (string-join (nreverse lines) "\n") "\n\n"
                  (if ok "Setup looks good.\n"
                    "Setup incomplete -- see [FAIL] lines above.\n")))
        (goto-char (point-min))
        (special-mode))
      (pop-to-buffer buf))))

(defun wire--target-live-p ()
  "Return non-nil if `wire-target' still names a live Claude window."
  (and wire-target
       (let ((id (plist-get wire-target :id))
             (sock (plist-get wire-target :socket)))
         (seq-find (lambda (w) (and (equal (plist-get w :id) id)
                                    (equal (plist-get w :socket) sock)))
                   (wire--claude-windows)))))

(defun wire--ensure-target ()
  "Return a valid target, prompting/selecting as needed."
  (unless (wire--target-live-p)
    (when wire-target
      (setq wire-target nil)
      (message "wire: previous target is gone; pick another"))
    (wire-select-target))
  wire-target)

;;;; Context capture

(defun wire--project-root (file)
  "Best-effort project root for FILE."
  (let ((dir (file-name-directory (or file default-directory))))
    (or (when (and file (fboundp 'project-current))
          (when-let ((proj (project-current nil dir)))
            (expand-file-name (project-root proj))))
        (when (and file (fboundp 'vc-root-dir))
          (let ((default-directory dir)) (vc-root-dir)))
        (when file (locate-dominating-file file ".git"))
        dir)))

(defun wire--buffer-project-root ()
  "Project root of the current buffer's `default-directory', or nil.
Returns a directory only when it is a genuine VCS/project root, not
just an arbitrary directory, so fileless buffers outside a project
contribute no misleading `Project:' line."
  (let ((dir default-directory))
    (or (when (fboundp 'project-current)
          (when-let ((proj (project-current nil dir)))
            (expand-file-name (project-root proj))))
        (when (fboundp 'vc-root-dir)
          (let ((default-directory dir)) (vc-root-dir)))
        (locate-dominating-file dir ".git"))))

(defun wire--git-branch (root)
  "Current git branch at ROOT, or nil if ROOT is not a git repo.
On a detached HEAD, mirror what `git status' reports
\(\"HEAD detached at <short-sha>\")."
  (when root
    (let ((default-directory root))
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil
                                   "rev-parse" "--abbrev-ref" "HEAD"))
          (let ((branch (string-trim (buffer-string))))
            (if (not (string= branch "HEAD"))
                branch
              (erase-buffer)
              (if (zerop (call-process "git" nil t nil
                                       "rev-parse" "--short" "HEAD"))
                  (format "HEAD detached at %s" (string-trim (buffer-string)))
                "HEAD (detached)"))))))))

(defun wire--lang ()
  "Language hint for the current buffer's major mode."
  (replace-regexp-in-string "\\(-ts\\)?-mode\\'" ""
                            (symbol-name major-mode)))

(defun wire--region-end-line (beg end)
  "Line number of the last line touched by the region BEG..END.
A region ending at the very start of a line (i.e. just past a
newline) does not extend onto that next line, so it is not counted;
this keeps the `of N' totals consistent."
  (if (and (> end beg)
           (save-excursion (goto-char end) (bolp)))
      (line-number-at-pos (1- end))
    (line-number-at-pos end)))

(defun wire--context-at-point ()
  "Capture context for a dispatch.
With an active region, capture the region text and its line range; the
message is about that region.  Without one, capture only buffer
identity --- the message is about the whole file or buffer.  File-backed
buffers carry a project/branch/file reference; fileless buffers carry
the project root and branch (if any), the buffer name, and the mode."
  (let* ((reg (use-region-p))
         (beg (and reg (region-beginning)))
         (end (and reg (region-end)))
         (code (and reg (buffer-substring-no-properties beg end)))
         (file (buffer-file-name)))
    (if file
        (let ((root (wire--project-root file)))
          (list :region reg
                :file file
                :project-root root
                :branch (wire--git-branch root)
                :rel-file (if root (file-relative-name file root) file)
                :beg-line (and reg (line-number-at-pos beg))
                :end-line (and reg (line-number-at-pos end))
                :code code
                :lang (wire--lang)))
      (let ((root (wire--buffer-project-root)))
        (list :region reg
              :file nil
              :project-root root
              :branch (wire--git-branch root)
              :buffer (buffer-name)
              :mode (symbol-name major-mode)
              :prog (and (derived-mode-p 'prog-mode) t)
              :beg-line (and reg (line-number-at-pos beg))
              :end-line (and reg (wire--region-end-line beg end))
              :total-lines (count-lines (point-min) (point-max))
              :code code
              :lang (wire--lang))))))

(defun wire--format-context (ctx)
  "Build the context block from CTX, adapting to the buffer kind."
  (if (plist-get ctx :file)
      (wire--format-file-context ctx)
    (wire--format-fileless-context ctx)))

(defun wire--format-file-context (ctx)
  "Build the context block for a file-backed buffer CTX.
With a region, includes the line range and the fenced region; without
one, the message is about the whole file, so only the project/branch/
file header is emitted."
  (let* ((branch (plist-get ctx :branch))
         (header (format "Project: %s\n%sFile: %s\n"
                         (plist-get ctx :project-root)
                         (if branch (format "Branch: %s\n" branch) "")
                         (plist-get ctx :rel-file))))
    (if (plist-get ctx :region)
        (let* ((beg (plist-get ctx :beg-line))
               (end (plist-get ctx :end-line))
               (lines (if (= beg end)
                          (format "Line: %d" beg)
                        (format "Lines: %d-%d" beg end))))
          (format "%s%s\n\n```%s\n%s\n```"
                  header lines (plist-get ctx :lang) (plist-get ctx :code)))
      ;; No region: header only.  Drop its trailing newline so the
      ;; caller's "\n\n" leaves exactly one blank line before point.
      (string-remove-suffix "\n" header))))

(defun wire--format-fileless-context (ctx)
  "Build the context block for a fileless buffer CTX.
Includes `Project:'/`Branch:' only for a genuine project root and
always the `Buffer:' name.  With a region, adds a `Focused: line(s) N
of M' provenance line and the fenced region, using the fence language
only for `prog-mode' buffers; without one, the message is about the
whole buffer.  A `Mode:' line is shown whenever the language is not
already carried by a fence."
  (let* ((root (plist-get ctx :project-root))
         (branch (plist-get ctx :branch))
         (region (plist-get ctx :region))
         ;; The language only appears inside the region fence, and only
         ;; for prog-mode buffers; otherwise `Mode:' carries the context.
         (fence-lang (and region (plist-get ctx :prog) (plist-get ctx :lang)))
         (header (concat
                  (when root (format "Project: %s\n" root))
                  (when branch (format "Branch: %s\n" branch))
                  (format "Buffer: %s\n" (plist-get ctx :buffer))
                  (unless fence-lang (format "Mode: %s\n" (plist-get ctx :mode))))))
    (if region
        (let* ((beg (plist-get ctx :beg-line))
               (end (plist-get ctx :end-line))
               (total (plist-get ctx :total-lines))
               (focused (if (= beg end)
                            (format "Focused: line %d of %d" beg total)
                          (format "Focused: lines %d-%d of %d" beg end total))))
          (format "%s%s\n\n```%s\n%s\n```"
                  header focused (or fence-lang "") (plist-get ctx :code)))
      ;; No region: header only.  Drop its trailing newline so the
      ;; caller's "\n\n" leaves exactly one blank line before point.
      (string-remove-suffix "\n" header))))

;;;; Annotation scaffolding
;;
;; The annotation buffer carries two buffer-only markers, both stripped
;; before the text reaches Claude:
;;
;;   * a `<CLAUDE TARGET: ...>' banner up top, so the user can confirm
;;     where the message is headed; and
;;   * a `<prompt>...</prompt>' block at the bottom, where the user types
;;     the request.
;;
;; `wire--compose-message' lifts the prompt contents to the front so the
;; sent message --- and thus Claude CLI's prompt history --- leads with
;; the request rather than the boilerplate context header.

(defun wire--format-target-block (target)
  "Render the one-line `<CLAUDE TARGET: ...>' banner for TARGET.
Shows the window title (the same text `wire-select-target' saw), as a
safeguard against dispatching to the wrong instance; falls back to the
full label when no title is available."
  (format "<CLAUDE TARGET: %s>"
          (or (plist-get target :title) (plist-get target :label))))

(defun wire--strip-target-block (text)
  "Strip a leading `<CLAUDE TARGET: ...>' banner line from TEXT.
The banner is for the user only and must never reach Claude."
  (replace-regexp-in-string
   "\\`[ \t\n]*<CLAUDE TARGET:[^>\n]*>[ \t]*\n?"
   "" text))

(defun wire--compose-message (text)
  "Build the message to send from annotation buffer TEXT.
Drop the target banner, then lift the trailing `<prompt>...</prompt>'
contents to the front so the request leads and the context block
follows --- giving Claude CLI history a meaningful summary line.  The
prompt tags are buffer-only scaffolding and are removed.  With an empty
prompt only the context is sent; if no prompt block is present the text
is sent as-is."
  (let ((body (string-trim (wire--strip-target-block text))))
    (if (string-match
         "\n*<prompt>[ \t]*\n?\\(\\(?:.\\|\n\\)*?\\)\n?[ \t]*</prompt>[ \t\n]*\\'"
         body)
        (let ((prompt (string-trim (match-string 1 body)))
              (context (string-trim (substring body 0 (match-beginning 0)))))
          (cond ((string-empty-p prompt) context)
                ((string-empty-p context) prompt)
                (t (concat prompt "\n\n" context))))
      body)))

;;;; Sending

(defun wire--flash-mode-line ()
  "Briefly invert the mode line to signal a successful dispatch."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(defun wire--send (target text)
  "Inject TEXT into TARGET's Claude prompt and submit it."
  (let ((sock (plist-get target :socket))
        (match (format "id:%d" (plist-get target :id))))
    (pcase-let ((`(,exit . ,out)
                 (wire--kitty sock text "send-text" "--match" match "--stdin"
                              "--bracketed-paste" wire-bracketed-paste)))
      (unless (and (integerp exit) (zerop exit))
        (user-error "wire: send-text failed (%s): %s" exit (string-trim out))))
    ;; Submit with a bare carriage return, sent raw so kitty does not
    ;; reinterpret it.
    (pcase-let ((`(,exit . ,out)
                 (wire--kitty sock "\r" "send-text" "--match" match "--stdin")))
      (unless (and (integerp exit) (zerop exit))
        (user-error "wire: submit failed (%s): %s" exit (string-trim out))))
    ;; Reached only when both kitty calls succeeded.
    (wire--flash-mode-line)))

;;;; Annotation buffer

(defvar-local wire--pending-target nil
  "Resolved target plist for the annotation buffer.")

(defvar-local wire--source-window nil
  "Window selected when the annotation buffer was created.")

(defun wire--restore-source-window ()
  "Clean up the annotation's window and return to the dispatch source.
Replays the window's `quit-restore' data: a window that
`wire-dispatch' created (via `pop-to-buffer') is deleted, restoring
the prior layout; a reused window gets its previous buffer back.
Runs from `kill-buffer-hook', so it covers confirm, abort and a
manual kill alike."
  (let ((win (get-buffer-window (current-buffer))))
    (when win (quit-restore-window win)))
  (when (window-live-p wire--source-window)
    (select-window wire--source-window)))

(defvar wire-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'wire-annotation-confirm)
    (define-key map (kbd "C-<return>") #'wire-annotation-confirm)
    (define-key map (kbd "C-c C-k") #'wire-annotation-abort)
    map)
  "Keymap for `wire-annotation-mode'.")

(defvar wire-post-dispatch-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'wire-visit-target)
    map)
  "Transient keymap active for the one key after a dispatch.
TAB focuses the target just sent to; any other key dismisses the map
and behaves normally.")

(defface wire-target-banner
  '((t :inherit font-lock-warning-face))
  "Face for the `<CLAUDE TARGET: ...>' banner in the annotation buffer."
  :group 'wire)

(defvar wire--font-lock-keywords
  '(("^<CLAUDE TARGET:[^>\n]*>$" 0 'wire-target-banner t))
  "Font-lock keywords highlighting the target banner.
The override flag keeps the banner from being restyled by the parent
mode's own fontification (e.g. `gfm-mode' treating it as an HTML tag).")

(defun wire--annotation-setup ()
  "Shared setup for the annotation buffer's major mode."
  (font-lock-add-keywords nil wire--font-lock-keywords)
  (setq header-line-format
        (substitute-command-keys
         "Edit the message, then \\[wire-annotation-confirm] to send, \
\\[wire-annotation-abort] to cancel")))

;; Derive from `gfm-mode' when markdown-mode is available so the fenced
;; code block is syntax-highlighted natively; fall back to `text-mode'.
(if (require 'markdown-mode nil t)
    (define-derived-mode wire-annotation-mode gfm-mode "Wire-Annotation"
      "Major mode for editing the message before it is wired to Claude."
      (setq-local markdown-fontify-code-blocks-natively t)
      (wire--annotation-setup))
  (define-derived-mode wire-annotation-mode text-mode "Wire-Annotation"
    "Major mode for editing the message before it is wired to Claude."
    (wire--annotation-setup)))

;; Defined conditionally above, so the compiler can't see it statically.
(declare-function wire-annotation-mode "wire")

(defun wire-annotation-confirm ()
  "Compose and send the annotation to the target Claude.
The `<prompt>' contents lead the message; the target banner and prompt
tags are dropped.  See `wire--compose-message'."
  (interactive)
  (let ((text (wire--compose-message (buffer-string)))
        (target wire--pending-target))
    (when (string-empty-p text)
      (user-error "wire: nothing to send"))
    (wire--send target text)
    (let ((label (plist-get target :label)))
      (kill-buffer (current-buffer))
      ;; Offer a one-key follow-up: TAB visits the target just sent to.
      (set-transient-map wire-post-dispatch-map)
      (message "wire: sent to Claude [%s]  (TAB: visit target)" label))))

(defun wire-annotation-abort ()
  "Abandon the annotation without sending."
  (interactive)
  (kill-buffer (current-buffer))
  (message "wire: cancelled"))

;;;; Main command

;;;###autoload
(defun wire-dispatch ()
  "Edit and send a message about the region (or whole buffer) to Claude.
Pops a buffer pre-filled with a context header --- project and branch,
the file or buffer name, and, when a region is active, its line range
and the region text fenced.  With no active region the message is about
the whole file/buffer and no code is included.  Type the request in the
`<prompt>' block; on send it is lifted ahead of the context so Claude
CLI history shows a meaningful summary line.  Point starts inside the
prompt block.  Prompts for a target the first time, or whenever the
previous target is gone."
  (interactive)
  (let ((target (wire--ensure-target))
        (ctx (wire--context-at-point)))
    (unless target (user-error "wire: no target selected"))
    ;; The region has been captured; drop it in the source buffer.
    (deactivate-mark)
    (let ((source (selected-window))
          (buf (generate-new-buffer "*wire annotation*")))
      (with-current-buffer buf
        (wire-annotation-mode)
        ;; A leading blank line, the target banner, the context block,
        ;; then an empty `<prompt>' block with point inside ready for the
        ;; request.  The banner and tags are stripped on send, and the
        ;; prompt is lifted to the front; see `wire--compose-message'.
        (insert "\n"
                (wire--format-target-block target) "\n\n"
                (wire--format-context ctx) "\n\n"
                "<prompt>\n")
        (save-excursion (insert "\n</prompt>\n"))
        (setq wire--pending-target target
              wire--source-window source)
        ;; Return to the source window however the buffer is killed
        ;; (confirm, abort, or a manual C-x k).
        (add-hook 'kill-buffer-hook #'wire--restore-source-window nil t))
      (pop-to-buffer buf))))

;;;###autoload
(defun wire-visit-target ()
  "Focus the terminal window of the current Claude target.
Raises the kitty window (and its tab) running the selected Claude
instance; on X11 this also brings kitty's OS window to the foreground.
Prompts for a target the first time, or whenever the previous one is
gone."
  (interactive)
  (let ((target (wire--ensure-target)))
    (unless target (user-error "wire: no target selected"))
    (let ((sock (plist-get target :socket))
          (id (plist-get target :id)))
      (pcase-let ((`(,exit . ,out)
                   (wire--kitty sock nil "focus-window"
                                "--match" (format "id:%d" id))))
        (unless (and (integerp exit) (zerop exit))
          (user-error "wire: focus failed (%s): %s" exit (string-trim out))))
      (message "wire: focused Claude [%s]" (plist-get target :label)))))

;;;; Minor mode

(defvar wire-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c y y") #'wire-dispatch)
    (define-key map (kbd "C-c y s") #'wire-select-target)
    (define-key map (kbd "C-c y l") #'wire-list-instances)
    (define-key map (kbd "C-c y v") #'wire-visit-target)
    (define-key map (kbd "C-c y d") #'wire-doctor)
    map)
  "Keymap for `wire-mode'.")

;;;###autoload
(define-minor-mode wire-mode
  "Minor mode to wire annotated regions to a running Claude instance."
  :lighter " Wire"
  :keymap wire-mode-map)

(defun wire--turn-on ()
  "Enable `wire-mode' unless in the minibuffer."
  (unless (minibufferp)
    (wire-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-wire-mode
  wire-mode wire--turn-on
  "Globalized `wire-mode': enable wire keybindings in all buffers.")

(provide 'wire)
;;; wire.el ends here
