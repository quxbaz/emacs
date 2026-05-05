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
            (insert ";; bottom\n"
                    (mapconcat #'prin1-to-string entries "\n")
                    "\n;; top\n")))
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
    (remove-hook 'post-command-hook #'my/calc-debug--post-command t)
    (when-let ((win (get-buffer-window my/calc-debug-buffer)))
      (delete-window win))))

(provide 'my/calc/debug)
