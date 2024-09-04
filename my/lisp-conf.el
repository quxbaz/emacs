;; Lisp config
;;
;;


;;
;; # All Lisp dialects config

;; CLIPBOARD
;; slime-repl-mode-hook

;; TODO: Add binding to show SLIME buffer.

;; TODO: Make this work with all Lisp modes.
(dolist (map (list emacs-lisp-mode-map lisp-mode-map))
  ;; TODO: Only use these for emacs-lisp-mode.
  (keymap-set map "C-c C-c" 'my/eval-dwim)
  (keymap-set map "C-c C-." 'my/eval-here)
  (keymap-set map "C-c C-x" 'my/eval-kill-ring)
  ;;

  (keymap-set map "M-/" 'completion-at-point)
  (keymap-set map "C-M-i" 'dabbrev-expand)
  (keymap-set map "M-n" 'my/forward-sexp)
  (keymap-set map "M-p" 'backward-sexp)
  (keymap-set map "C-M-." 'my/mark-list-command)
  (keymap-set map "M-w" 'my/lisp-kill-ring-save-dwim)
  (keymap-set map "M-<return>" 'my/duplicate-list)
  (keymap-set map "C-k" 'my/lisp-kill-dwim)
  (keymap-set map "M-k" 'my/kill-list)
  (keymap-set map "M-(" 'my/wrap-sexp)
  (keymap-set map "S-<return>" 'my/close-round-and-newline)
  (keymap-set map "C-<return>" 'my/open-new-round)
  (keymap-set map "C-c C-s" 'paredit-splice-sexp)
  (keymap-set map "C-c C-o" 'paredit-raise-sexp)
  (keymap-set map "C-t" 'my/lisp-transpose-chars)
  (keymap-set map "C-;" 'my/lisp-comment-dwim))

;; TODO: Make this work with all Lisp modes.
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (autopair-mode 0)
                                  (aggressive-indent-mode t)
                                  (paredit-mode t)
                                  (rainbow-blocks-mode t)))

;; TODO: Add SLIME mode maps.

(eval-after-load 'paredit '(progn
                             (keymap-set paredit-mode-map "M-s" 'save-buffer)
                             (keymap-set paredit-mode-map "M-r" 'my/query-replace-dwim)
                             (keymap-set paredit-mode-map "M-;" 'my/comment-block)
                             (keymap-set paredit-mode-map "M-<up>" 'my/transpose-line)
                             (keymap-set paredit-mode-map "M-<down>" 'my/transpose-line-down)
                             (keymap-set paredit-mode-map "C-k" 'my/lisp-kill-dwim)
                             (keymap-set paredit-mode-map "M-(" 'my/wrap-sexp)))


;;
;; # Common Lisp config
(setq inferior-lisp-program (executable-find "sbcl"))


;;
;; # Emacs Lisp (Elisp) config
;;
