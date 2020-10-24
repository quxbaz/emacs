;; TODO
;; - Highlight all in (), {}, [], '', ""
;; - Fix defuns/my-comment-line
;; - Go over rest of defuns.
;; - defuns/my-delete-in-container

(load-file "~/.emacs.d/my/requires.el")
(load-file "~/.emacs.d/my/conf.el")
(load-file "~/.emacs.d/my/defuns.el")
(load-file "~/.emacs.d/my/bindings.el")
(load-file "~/.emacs.d/my/hooks.el")

;; Auto settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "4031c1ea0bb235b75a048bd92f3bf3aa984c9f7cc5b408f00f62ed99a6eecc09" default))
 '(package-selected-packages
   '(clojure-mode tide yasnippet wrap-region tern-auto-complete rainbow-mode neotree magit flycheck-flow color-theme autopair))
 '(quack-fontify-style 'emacs)
 '(quack-programs
   '("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))
 '(safe-local-variable-values
   '((name . box)
     (name . list)
     (name . controls)
     (name . reset)
     (name . __here__)
     (name . screen)))
 '(scss-compile-at-save nil)
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t nil))))
