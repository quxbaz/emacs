;;
;; All Lisp dialects config

;; TODO: Make this work with all Lisp modes.
(keymap-set emacs-lisp-mode-map "C-M-." 'my/mark-list-command)
(keymap-set emacs-lisp-mode-map "M-<return>" 'my/duplicate-list)
(keymap-set emacs-lisp-mode-map "M-/" 'completion-at-point)
(keymap-set emacs-lisp-mode-map "C-M-i" 'dabbrev-expand)
(keymap-set emacs-lisp-mode-map "M-n" 'my/forward-sexp)
(keymap-set emacs-lisp-mode-map "M-p" 'backward-sexp)
(keymap-set emacs-lisp-mode-map "C-c C-c" 'my/eval-dwim)
(keymap-set emacs-lisp-mode-map "C-c C-." 'my/eval-here)
(keymap-set emacs-lisp-mode-map "C-c C-x" 'my/eval-kill-ring)
(keymap-set emacs-lisp-mode-map "M-w" 'my/lisp-kill-ring-save-dwim)
(keymap-set emacs-lisp-mode-map "C-k" 'my/lisp-kill-dwim)
(keymap-set emacs-lisp-mode-map "M-k" 'my/kill-list)
(keymap-set emacs-lisp-mode-map "M-9"  'backward-up-list)
(keymap-set emacs-lisp-mode-map "S-<return>" 'my/close-round-and-newline)
(keymap-set emacs-lisp-mode-map "C-<return>" 'my/open-new-round)
(keymap-set emacs-lisp-mode-map "M-("  'my/wrap-sexp)
(keymap-set emacs-lisp-mode-map "C-c C-s" 'paredit-splice-sexp)
(keymap-set emacs-lisp-mode-map "C-c C-o" 'paredit-raise-sexp)
(keymap-set emacs-lisp-mode-map "C-t" 'my/lisp-transpose-chars)
(keymap-set emacs-lisp-mode-map "C-;" 'my/lisp-comment-dwim)

;; TODO: Make this work with all Lisp modes.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (autopair-mode 0)
            (aggressive-indent-mode t)
            (paredit-mode t)
            (rainbow-blocks-mode t)))

(eval-after-load 'paredit
  '(progn
     (keymap-set paredit-mode-map "M-s" 'save-buffer)
     (keymap-set paredit-mode-map "M-r" 'my/query-replace-dwim)
     (keymap-set paredit-mode-map "M-;" 'my/comment-block)
     (keymap-set paredit-mode-map "M-<up>" 'my/transpose-line)
     (keymap-set paredit-mode-map "M-<down>" 'my/transpose-line-down)
     (keymap-set paredit-mode-map "C-k" 'my/lisp-kill-dwim)
     (keymap-set paredit-mode-map "M-(" 'my/wrap-sexp)))


;;
;; Common Lisp config
(setq inferior-lisp-program (executable-find "sbcl"))


;;
;; Emacs Lisp (Elisp) config
;;
