;; -*- lexical-binding: t; -*-
;;
;; Lisp keybindings
;;
;; The my/bind / my/apply-when / my/apply-bindings helpers live in my/util.el.


;;
;; # Shared Lisp keymap
;;
;; Custom keybindings for all Lisp modes.
(defconst my/lisp-shared-bindings
  '(("C-c C-b"     . nil)
    ("M-."         . my/mark-context)
    ("C-\\"        . my/match-outside-delimiter)
    ("M-,"         . my/call-macro-dwim)
    ("M-/"         . completion-at-point)
    ("C-M-i"       . dabbrev-expand)
    ("M-n"         . my/forward-sexp)
    ("M-p"         . backward-sexp)
    ("C-M-."       . my/mark-list-command)
    ("M-w"         . my/lisp-kill-ring-save-dwim)
    ("M-<return>"  . my/duplicate-list)
    ("C-k"         . my/lisp-kill-dwim)
    ("M-k"         . my/kill-list)
    ("M-o"         . my/wrap-sexp)
    ("S-<return>"  . my/close-round-and-newline)
    ("C-<return>"  . my/open-new-round)
    ("C-c C-s"     . paredit-splice-sexp)
    ("C-c C-o"     . paredit-raise-sexp)
    ("C-t"         . my/lisp-transpose-chars)
    ("C-;"         . my/lisp-comment-dwim))
  "Keybindings shared by all Lisp modes.")

(defun my/apply-shared-bindings (&rest keymaps)
  "Apply the shared Lisp keybindings to each of KEYMAPS."
  (dolist (keymap keymaps)
    (my/apply-bindings keymap my/lisp-shared-bindings)))

(my/apply-shared-bindings emacs-lisp-mode-map lisp-mode-map)

;;
;; # Emacs Lisp (Elisp) keybindings
(my/bind :now emacs-lisp-mode-map
  "C-c C-c" 'my/eval-dwim
  "C-c C-k" (my/with-prefix 'my/eval-dwim '(16))
  "<f5>"    'my/eval-buffer-show-messages
  "C-c C-." 'my/eval-here
  "C-c RET" 'my/macroexpand-here)

;;
;; # Paredit keybindings
;;
;; Override default paredit keymap.
(my/bind (:after paredit) paredit-mode-map
  "M-("      (my/cmd nil)
  "\\"       'my/key-backslash
  "C-j"      'newline-and-indent
  "M-s"      'save-buffer
  "M-r"      'my/query-replace-dwim
  "M-;"      'my/comment-block
  "M-<up>"   'my/transpose-line
  "M-<down>" 'my/transpose-line-down
  "C-k"      'my/lisp-kill-dwim
  "C-M-b"    'magit-blob-mode)

;;
;; # SLIME and SLIME REPL keybindings
;; (keymap-set lisp-mode-map "C-c C-l" 'slime-switch-to-output-buffer)
;;
;; Bind in slime-mode-map, not lisp-mode-map: slime-mode is a minor mode, so its
;; keymap shadows the major-mode map's C-c C-c / C-c C-k when SLIME is active.
(my/bind (:after slime) slime-mode-map
  "C-c C-c" 'my/slime-eval-dwim
  "C-c C-k" (my/with-prefix 'my/slime-eval-dwim '(16))
  "M-."     'my/mark-context
  "M-,"     'my/call-macro-dwim
  "C-h C-h" 'my/slime-help-dwim
  "C-h C-o" 'slime-hyperspec-lookup
  "C-c C-x" 'slime-compile-defun
  "C-c C-." 'slime-eval-last-expression
  "C-c C-l" 'my/slime-load-this-file)

;; Temporary, until you get ELisp completion-at-point working.
;; (my/bind (:after slime) slime-mode-indirect-map "M-/" 'dabbrev-expand)
(my/bind (:after slime) slime-mode-indirect-map
  "M-/" 'hippie-expand)

;; When the SLIME REPL is activated, it also modifies slime-mode-map, so we need
;; to re-apply our custom slime-mode-map keybindings from slime-repl-mode-hook
;; -- and because it re-applies on every activation, this must stay a hook.
(my/apply-when '(:hook slime-repl-mode-hook)
               (lambda ()
                 (my/apply-shared-bindings slime-mode-map slime-repl-mode-map)
                 (my/apply-bindings slime-repl-mode-map
                                    '(("<backspace>" . paredit-backward-delete)
                                      ;; ("M-/" . dabbrev-expand)
                                      ("M-/"         . hippie-expand)
                                      ("M-p"         . slime-repl-previous-prompt)
                                      ("M-n"         . slime-repl-next-prompt)))))
