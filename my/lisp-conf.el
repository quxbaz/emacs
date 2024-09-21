;; Lisp config
;;


;;
;; # Shared config

;; Custom keybindings for all Lisp modes.
(defun my/set-lisp-keymap (keymap)
  (keymap-set keymap "C-c C-b" nil)
  (keymap-set keymap "M-/" 'completion-at-point)
  (keymap-set keymap "C-M-i" 'dabbrev-expand)
  (keymap-set keymap "M-n" 'my/forward-sexp)
  (keymap-set keymap "M-p" 'backward-sexp)
  (keymap-set keymap "C-M-." 'my/mark-list-command)
  (keymap-set keymap "M-w" 'my/lisp-kill-ring-save-dwim)
  (keymap-set keymap "M-<return>" 'my/duplicate-list)
  (keymap-set keymap "C-k" 'my/lisp-kill-dwim)
  (keymap-set keymap "M-k" 'my/kill-list)
  (keymap-set keymap "C-w" 'my/wrap-sexp)
  (keymap-set keymap "S-<return>" 'my/close-round-and-newline)
  (keymap-set keymap "C-<return>" 'my/open-new-round)
  (keymap-set keymap "C-c C-s" 'paredit-splice-sexp)
  (keymap-set keymap "C-c C-o" 'paredit-raise-sexp)
  (keymap-set keymap "C-t" 'my/lisp-transpose-chars)
  (keymap-set keymap "C-;" 'my/lisp-comment-dwim))

(my/set-lisp-keymap emacs-lisp-mode-map)
(my/set-lisp-keymap lisp-mode-map)

;; When the SLIME REPL is activated, it modifies slime-mode-map, so we need to
;; set our custom slime-mode-map keybindings from slime-repl-mode-hook.
(add-hook 'slime-repl-mode-hook (lambda ()
                                  (my/set-lisp-keymap slime-mode-map)
                                  (my/set-lisp-keymap slime-repl-mode-map)))

;; Override default paredit keymap.
(eval-after-load 'paredit '(progn
                             (keymap-set paredit-mode-map "C-j" 'newline-and-indent)
                             (keymap-set paredit-mode-map "M-s" 'save-buffer)
                             (keymap-set paredit-mode-map "M-r" 'my/query-replace-dwim)
                             (keymap-set paredit-mode-map "M-;" 'my/comment-block)
                             (keymap-set paredit-mode-map "M-<up>" 'my/transpose-line)
                             (keymap-set paredit-mode-map "M-<down>" 'my/transpose-line-down)
                             (keymap-set paredit-mode-map "C-k" 'my/lisp-kill-dwim)))

;; Mode hooks
(dolist (mode-hook '(emacs-lisp-mode-hook lisp-mode-hook slime-repl-mode-hook))
  (add-hook mode-hook (lambda ()
                        (autopair-mode 0)
                        (aggressive-indent-mode t)
                        (rainbow-blocks-mode t)
                        (paredit-mode t))))

;;
;; # Common Lisp config
(setq inferior-lisp-program (executable-find "sbcl"))
(keymap-set lisp-mode-map "C-c C-<return>" 'my/visit-slime-repl)


;;
;; # Emacs Lisp (Elisp) config
(keymap-set emacs-lisp-mode-map "C-c C-c" 'my/eval-dwim)
(keymap-set emacs-lisp-mode-map "C-c C-." 'my/eval-here)
(keymap-set emacs-lisp-mode-map "C-c C-x" 'my/eval-kill-ring)
