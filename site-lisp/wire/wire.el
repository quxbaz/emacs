;;; wire.el --- Send annotated regions to a running Claude instance  -*- lexical-binding: t; -*-

;; Author: David
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.0

;;; Commentary:

;; Wire sends the active region (or the line at point) plus a short
;; annotation to a *running* Claude Code session living in a kitty
;; terminal window.
;;
;; The message it builds includes the project root, the file, the line
;; range, and the code itself, fenced for the buffer's language.  It is
;; injected into Claude's prompt via kitty's remote control protocol
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
;;   M-x wire-mode               ; enable in a source buffer
;;   M-x wire-select-target      ; pick which Claude window to target
;;   mark a region, M-x wire-dispatch
;;   edit the pre-filled message, C-c C-c   ; send (C-c C-k cancels)
;;
;; Default keys under `wire-mode': C-c c c (dispatch), C-c c s
;; (select-target), C-c c l (list instances).

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
      (display-buffer buf))))

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

(defun wire--lang ()
  "Language hint for the current buffer's major mode."
  (replace-regexp-in-string "\\(-ts\\)?-mode\\'" ""
                            (symbol-name major-mode)))

(defun wire--context-at-point ()
  "Capture the region (or current line) and surrounding context."
  (let* ((reg (use-region-p))
         (beg (if reg (region-beginning) (line-beginning-position)))
         (end (if reg (region-end) (line-end-position)))
         (file (buffer-file-name))
         (root (wire--project-root file)))
    (list :file (or file (buffer-name))
          :project-root root
          :rel-file (if (and file root)
                        (file-relative-name file root)
                      (or file (buffer-name)))
          :beg-line (line-number-at-pos beg)
          :end-line (line-number-at-pos end)
          :code (buffer-substring-no-properties beg end)
          :lang (wire--lang))))

(defun wire--format-context (ctx)
  "Build the context block (project, file, line range, code) from CTX."
  (let* ((beg (plist-get ctx :beg-line))
         (end (plist-get ctx :end-line))
         (lines (if (= beg end)
                    (format "line: %d" beg)
                  (format "lines: %d-%d" beg end))))
    (format "Project: %s\nFile: %s\n%s\n\n```%s\n%s\n```"
            (plist-get ctx :project-root)
            (plist-get ctx :rel-file)
            lines
            (plist-get ctx :lang)
            (plist-get ctx :code))))

;;;; Sending

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
        (user-error "wire: submit failed (%s): %s" exit (string-trim out))))))

;;;; Annotation buffer

(defvar-local wire--pending-target nil
  "Resolved target plist for the annotation buffer.")

(defvar wire-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'wire-annotation-confirm)
    (define-key map (kbd "C-c C-k") #'wire-annotation-abort)
    map)
  "Keymap for `wire-annotation-mode'.")

(define-derived-mode wire-annotation-mode text-mode "Wire-Annotation"
  "Major mode for editing the message before it is wired to Claude."
  (setq header-line-format
        (substitute-command-keys
         "Edit the message, then \\[wire-annotation-confirm] to send, \
\\[wire-annotation-abort] to cancel")))

(defun wire-annotation-confirm ()
  "Send the buffer contents verbatim to the target Claude."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (target wire--pending-target))
    (when (string-empty-p text)
      (user-error "wire: nothing to send"))
    (wire--send target text)
    (let ((label (plist-get target :label)))
      (kill-buffer (current-buffer))
      (message "wire: sent to Claude [%s]" label))))

(defun wire-annotation-abort ()
  "Abandon the annotation without sending."
  (interactive)
  (kill-buffer (current-buffer))
  (message "wire: cancelled"))

;;;; Main command

;;;###autoload
(defun wire-dispatch ()
  "Edit and send the region (or current line) to Claude.
Pops a buffer pre-filled with the full message --- project, file,
line range and the code block --- which you can edit freely; the
buffer contents are sent verbatim.  Point starts on the blank last
line, ready for a note.  Prompts for a target the first time, or
whenever the previous target is gone."
  (interactive)
  (let ((target (wire--ensure-target))
        (ctx (wire--context-at-point)))
    (unless target (user-error "wire: no target selected"))
    (let ((buf (generate-new-buffer "*wire annotation*")))
      (with-current-buffer buf
        (wire-annotation-mode)
        ;; One blank line on top, the context block, then two blank lines
        ;; with point on the last so a note can be appended at the bottom.
        (insert "\n" (wire--format-context ctx) "\n\n")
        (goto-char (point-max))
        (setq wire--pending-target target))
      (pop-to-buffer buf))))

;;;; Minor mode

(defvar wire-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c c") #'wire-dispatch)
    (define-key map (kbd "C-c c s") #'wire-select-target)
    (define-key map (kbd "C-c c l") #'wire-list-instances)
    map)
  "Keymap for `wire-mode'.")

;;;###autoload
(define-minor-mode wire-mode
  "Minor mode to wire annotated regions to a running Claude instance."
  :lighter " Wire"
  :keymap wire-mode-map)

(provide 'wire)
;;; wire.el ends here
