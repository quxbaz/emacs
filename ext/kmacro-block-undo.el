;; Source: https://old.reddit.com/r/emacs/comments/rlli0u/whats_your_favorite_defadvice/hpic51a/

(defun block-undo (fn &rest args)
  (let ((marker (prepare-change-group)))
    (unwind-protect (apply fn args)
      (undo-amalgamate-change-group marker))))

(dolist (fn '(kmacro-call-macro
              kmacro-exec-ring-item
              dot-mode-execute
              apply-macro-to-region-lines))
  (advice-add fn :around #'block-undo))
