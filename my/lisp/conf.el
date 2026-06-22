;; -*- lexical-binding: t; -*-
;;
;; Lisp config, variables, modes


;;
;; # Common Lisp config
(setq inferior-lisp-program (executable-find "sbcl"))
;; That will make sure SLIME plays nice with anything you do with Quicklisp in
;; the future (it will see all of the libraries you install, completion will
;; work etc).
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

;;
;; # SLIME and SLIME REPL config
(setq common-lisp-hyperspec-root (expand-file-name "~/common-lisp/hyperspec/HyperSpec/"))

;;
;; # rainbow-blocks: fix stale highlighting on first display
;;
;; `rainbow-blocks-propertize-region' throws away the START/END that jit-lock
;; hands it and instead recomputes the visible region from (window-start) and
;; (window-end).  (window-end)'s value is cached from the last completed
;; redisplay, so when jit-lock fontifies before display has settled (a buffer
;; first appearing, scrolling, or fontifying a buffer that isn't in the selected
;; window) the bounds are stale and nothing gets colored -- until an edit forces
;; a redisplay and a refontify.
;;
;; This override honors the region jit-lock provides, the way the closely
;; related `rainbow-delimiters' does.  Depth is still computed absolutely via
;; `rainbow-blocks-depth' (syntax-ppss from buffer start), so coloring an
;; arbitrary chunk stays correct.
(with-eval-after-load 'rainbow-blocks
  (defun my/rainbow-blocks-propertize-region (start end)
    "Like `rainbow-blocks-propertize-region', but honor jit-lock's START/END."
    (setq rainbow-blocks-escaped-char-predicate
          (cdr (assoc major-mode rainbow-blocks-escaped-char-predicate-list)))
    (save-excursion
      (goto-char start)
      (let ((depth (rainbow-blocks-depth start)))
        (while (and (< (point) end)
                    (re-search-forward rainbow-blocks-delim-regex end t))
          (backward-char)
          (unless (rainbow-blocks-char-ineligible-p (point))
            (let ((delim (char-after (point))))
              (cond ((eq ?\( delim)
                     (setq depth (1+ depth))
                     (rainbow-blocks-apply-color "paren" depth (point)))
                    ((eq ?\) delim)
                     (setq depth (or (and (<= depth 0) 0) (1- depth))))
                    ((eq ?\[ delim)
                     (setq depth (1+ depth))
                     (rainbow-blocks-apply-color "bracket" depth (point)))
                    ((eq ?\] delim)
                     (setq depth (or (and (<= depth 0) 0) (1- depth))))
                    ((eq ?\{ delim)
                     (setq depth (1+ depth))
                     (rainbow-blocks-apply-color "brace" depth (point)))
                    ((eq ?\} delim)
                     (setq depth (or (and (<= depth 0) 0) (1- depth)))))))
          (forward-char)))))

  (advice-add 'rainbow-blocks-propertize-region :override
              #'my/rainbow-blocks-propertize-region))
