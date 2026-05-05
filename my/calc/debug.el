;; -*- lexical-binding: t; -*-
;;
;; Calc debug utilities


(defconst my/calc-debug-buffer "*calc-debug*")

(defvar-local my/calc-debug--source-buffer nil)

(defun my/calc-debug--quit ()
  (interactive)
  (when (buffer-live-p my/calc-debug--source-buffer)
    (with-current-buffer my/calc-debug--source-buffer
      (my/calc-debug-mode -1))))

(defun my/calc-debug--print-stack ()
  (interactive)
  (when (buffer-live-p my/calc-debug--source-buffer)
    (let ((stack (buffer-local-value 'calc-stack my/calc-debug--source-buffer)))
      (with-current-buffer (current-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (let* ((entries (cl-remove-if (lambda (e) (eq (car e) 'top-of-stack)) stack))
                 (entries (reverse entries)))
            (insert ";; Bottom of stack ▼\n"
                    (mapconcat #'prin1-to-string entries "\n")
                    "\n;; Top of stack ▲\n")))
        (when-let ((win (get-buffer-window (current-buffer))))
          (with-selected-window win
            (goto-char (point-max))
            (forward-line -1)
            (recenter-top-bottom -1)))))))

(defun my/calc-debug--output (expr)
  (let ((s (prin1-to-string expr)))
    (with-current-buffer (get-buffer-create my/calc-debug-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert s "\n"))
      (when-let ((win (get-buffer-window my/calc-debug-buffer)))
        (with-selected-window win
          (goto-char (point-max))
          (forward-line -1)
          (recenter-top-bottom -1))))))

(defvar-local my/calc-debug--hl-overlay nil)

(defun my/calc-debug--highlight-update ()
  "Highlight the subexpression at point using an overlay."
  (when (and (derived-mode-p 'calc-mode) (not (minibufferp)))
    (require 'calc-sel)
    (require 'calc-yank)
    (when (overlayp my/calc-debug--hl-overlay)
      (delete-overlay my/calc-debug--hl-overlay)
      (setq my/calc-debug--hl-overlay nil))
    (let ((idx (calc-locate-cursor-element (point))))
      (when (> idx 0)
        (calc-prepare-selection idx)
        (let* ((found (calc-find-selected-part))
               (entry calc-selection-cache-entry))
          (when found
            (let* ((temp-entry (list (car entry) (cadr entry) found))
                   (calc-highlight-selections-with-faces t)
                   (calc-show-selections nil)
                   (calc-prepared-composition calc-selection-cache-comp)
                   (calc-selection-cache-default-entry temp-entry)
                   (saved-cache-entry calc-selection-cache-entry)
                   (s (math-format-stack-value temp-entry))
                   (_ (setq calc-selection-cache-entry saved-cache-entry))
                   (entry-start (save-excursion
                                  (calc-cursor-stack-index idx)
                                  (point))))
              (let ((pos 0) sel-start sel-end)
                (while (< pos (length s))
                  (let ((next (next-single-property-change pos 'face s (length s))))
                    (when (eq (get-text-property pos 'face s) 'calc-selected-face)
                      (unless sel-start (setq sel-start pos))
                      (setq sel-end next))
                    (setq pos next)))
                (when sel-start
                  (setq my/calc-debug--hl-overlay
                        (make-overlay (+ entry-start sel-start)
                                      (+ entry-start sel-end)))
                  (overlay-put my/calc-debug--hl-overlay
                               'face 'highlight))))))))))

(define-minor-mode my/calc-debug-highlight-mode
  "Highlight subexpression at point as cursor moves."
  :lighter " HL"
  (if my/calc-debug-highlight-mode
      (add-hook 'post-command-hook #'my/calc-debug--highlight-update nil t)
    (remove-hook 'post-command-hook #'my/calc-debug--highlight-update t)
    (when (overlayp my/calc-debug--hl-overlay)
      (delete-overlay my/calc-debug--hl-overlay)
      (setq my/calc-debug--hl-overlay nil))))

(defun my/calc-debug-print-line ()
  "Print structural representation of calc expression at point."
  (interactive)
  (require 'calc-yank)
  (let* ((idx (calc-locate-cursor-element (point)))
         (expr (and (> idx 0) (calc-top idx))))
    (if expr
        (my/calc-debug--output expr)
      (message "No calc expression at point"))))

(defun my/calc-debug--post-command ()
  (require 'calc-yank)
  (let* ((idx (calc-locate-cursor-element (point)))
         (expr (and (> idx 0) (calc-top idx))))
    (when expr
      (my/calc-debug--output expr))))

(define-minor-mode my/calc-debug-mode
  "Continuously show structural representation of expression at point."
  :lighter " CDbg"
  (if my/calc-debug-mode
      (progn
        (my/calc-debug-highlight-mode 1)
        (add-hook 'post-command-hook #'my/calc-debug--post-command nil t)
        (let ((calc-buf (current-buffer)))
          (with-current-buffer (get-buffer-create my/calc-debug-buffer)
            (let ((inhibit-read-only t)) (erase-buffer))
            (emacs-lisp-mode)
            (rainbow-blocks-mode 1)
            (read-only-mode 1)
            (setq my/calc-debug--source-buffer calc-buf)
            (local-set-key (kbd "q") #'my/calc-debug--quit)
            (local-set-key (kbd "S") #'my/calc-debug--print-stack)))
        (display-buffer my/calc-debug-buffer
                        '(display-buffer-below-selected . ((window-height . 20)))))
    (my/calc-debug-highlight-mode -1)
    (remove-hook 'post-command-hook #'my/calc-debug--post-command t)
    (when-let ((win (get-buffer-window my/calc-debug-buffer)))
      (delete-window win))))

(add-hook 'calc-mode-hook #'my/calc-debug-highlight-mode)

(provide 'my/calc/debug)
