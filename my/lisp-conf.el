;; -*- lexical-binding: t; -*-


;; Lisp config


;;
;; # Shared Lisp config
;;
;; Custom keybindings for all Lisp modes.
(defun my/set-lisp-keymap (keymap)
  (keymap-set keymap "C-c C-b" nil)
  (keymap-set keymap "M-." 'my/mark-context)
  (keymap-set keymap "C-\\" 'my/match-outside-delimiter)
  (keymap-set keymap "M-," 'my/call-macro-dwim)
  (keymap-set keymap "M-/" 'completion-at-point)
  (keymap-set keymap "C-M-i" 'dabbrev-expand)
  (keymap-set keymap "M-n" 'my/forward-sexp)
  (keymap-set keymap "M-p" 'backward-sexp)
  (keymap-set keymap "C-M-." 'my/mark-list-command)
  (keymap-set keymap "M-w" 'my/lisp-kill-ring-save-dwim)
  (keymap-set keymap "M-<return>" 'my/duplicate-list)
  (keymap-set keymap "C-k" 'my/lisp-kill-dwim)
  (keymap-set keymap "M-k" 'my/kill-list)
  (keymap-set keymap "M-o" 'my/wrap-sexp)
  (keymap-set keymap "S-<return>" 'my/close-round-and-newline)
  (keymap-set keymap "C-<return>" 'my/open-new-round)
  (keymap-set keymap "C-c C-s" 'paredit-splice-sexp)
  (keymap-set keymap "C-c C-o" 'paredit-raise-sexp)
  (keymap-set keymap "C-t" 'my/lisp-transpose-chars)
  (keymap-set keymap "C-;" 'my/lisp-comment-dwim))

(my/set-lisp-keymap emacs-lisp-mode-map)
(my/set-lisp-keymap lisp-mode-map)

;; Apply hook to all Lisp and Lisp REPL modes.
(dolist (mode-hook '(emacs-lisp-mode-hook lisp-mode-hook slime-repl-mode-hook))
  (add-hook mode-hook (lambda ()
                        (autopair-mode -1)
                        (unless (eq mode-hook 'slime-repl-mode-hook)
                          (aggressive-indent-mode t))
                        (rainbow-blocks-mode t)
                        (paredit-mode t))))

;;
;; # Common Lisp config
(setq inferior-lisp-program (executable-find "sbcl"))
;; That will make sure SLIME plays nice with anything you do with Quicklisp in
;; the future (it will see all of the libraries you install, completion will
;; work etc).
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(keymap-set lisp-mode-map "C-c C-;" 'slime-switch-to-output-buffer)

;;
;; # Emacs Lisp (Elisp) config
(keymap-set emacs-lisp-mode-map "C-c C-c" 'my/eval-dwim)
(keymap-set emacs-lisp-mode-map "C-c C-." 'my/eval-here)
(keymap-set emacs-lisp-mode-map "C-c C-x" 'my/eval-kill-ring)

;;
;; # Paredit config
;;
;; Override default paredit keymap.
(eval-after-load 'paredit '(progn
                             (keymap-set paredit-mode-map "M-(" (my/cmd nil))
                             (keymap-set paredit-mode-map "\\" 'my/key-backslash)
                             (keymap-set paredit-mode-map "C-j" 'newline-and-indent)
                             (keymap-set paredit-mode-map "M-s" 'save-buffer)
                             (keymap-set paredit-mode-map "M-r" 'my/query-replace-dwim)
                             (keymap-set paredit-mode-map "M-;" 'my/comment-block)
                             (keymap-set paredit-mode-map "M-<up>" 'my/transpose-line)
                             (keymap-set paredit-mode-map "M-<down>" 'my/transpose-line-down)
                             (keymap-set paredit-mode-map "C-k" 'my/lisp-kill-dwim)
                             (keymap-set paredit-mode-map "C-M-b" 'magit-blob-mode)))

;;
;; # SLIME and SLIME REPL config
(setq common-lisp-hyperspec-root (expand-file-name "~/common-lisp/hyperspec/HyperSpec/"))

(defun my/slime-help-dwim ()
  "Displays documentation for the function at point."
  (interactive)
  (call-interactively 'slime-describe-function)
  ;; The window selection only works when a delay is used, hence run-with-idle-timer.
  (run-with-idle-timer 0.02 nil (lambda () (select-window (get-buffer-window "*slime-description*")))))

(eval-after-load 'slime '(progn
                           (keymap-set slime-mode-map "M-," 'my/call-macro-dwim)
                           (keymap-set slime-mode-map "C-h C-h" 'my/slime-help-dwim)
                           (keymap-set slime-mode-map "C-h C-o" 'slime-hyperspec-lookup)
                           (keymap-set slime-mode-map "C-c C-x" 'slime-compile-defun)
                           (keymap-set slime-mode-map "C-c C-c" 'slime-eval-defun)
                           (keymap-set slime-mode-map "C-c C-." 'slime-eval-last-expression)))

;; When the SLIME REPL is activated, it also modifies slime-mode-map, so we need
;; to set our custom slime-mode-map keybindings from slime-repl-mode-hook.
(add-hook 'slime-repl-mode-hook (lambda ()
                                  (my/set-lisp-keymap slime-mode-map)
                                  (my/set-lisp-keymap slime-repl-mode-map)
                                  (keymap-set slime-repl-mode-map "<backspace>" 'paredit-backward-delete)
                                  (keymap-set slime-repl-mode-map "M-p" 'slime-repl-previous-prompt)
                                  (keymap-set slime-repl-mode-map "M-n" 'slime-repl-next-prompt)))
