(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
; (when (< emacs-major-version 24)
  ; For important compatibility libraries like cl-lib
  ; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(require 'color-theme)
(load-theme 'wombat t)

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . javascript-mode))

(require 'flycheck-flow)
(setq flycheck-check-syntax-automatically '(mode-enabled idle-change))
(setq flycheck-idle-change-delay 0.5)

;; (setq flycheck-eslintrc "~/.eslintrc")

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(add-to-list 'load-path "~/.emacs.d/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(setq ac-auto-show-menu 0)

(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f4] 'neotree-toggle)

(add-hook 'neotree-mode-hook
  (lambda ()
    (local-set-key (kbd "TAB") 'neotree-enter)))

(autopair-global-mode) ;; enable autopair in all buffers

(add-hook 'autopair-mode-hook
  #'(lambda ()
      (push '(?{ . ?})
            (getf autopair-extra-pairs :code))))

(add-to-list 'load-path "~/.emacs.d/rainbow-mode")
(require 'rainbow-mode)

;; (add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-initialize)
(yas-load-directory "~/.emacs.d/snippets")

(add-to-list 'load-path "~/.emacs.d/el-get/wrap-region")
(require 'wrap-region)

(add-to-list 'load-path "~/.emacs.d/el-get/magit")
(require 'magit)

;; Allow local buffer set variable for all values for "buffer-name".
;; Source: http://stackoverflow.com/q/353246/376590
(defvar buffer-name nil)
(defun rename-buffer-if-necessary ()
  "Rename the current buffer according to the value of variable"
  (interactive)
  (if (and buffer-name (stringp buffer-name))
      (rename-buffer buffer-name)))
(add-hook 'find-file-hook 'rename-buffer-if-necessary)
(defadvice safe-local-variable-p (after allow-buffer-name (sym val) activate)
  (if (eq sym 'buffer-name)
      (setq ad-return-value t)))

(setq-default inhibit-startup-message t)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(column-number-mode t)
(global-hi-lock-mode t)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq backup-directory-alist `(("." . "~/.emacs.backups")))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(ido-mode t)

(iswitchb-mode t)
;; (partial-completion-mode t)
;; (set-face-attribute 'default nil :font "Inconsolata-11")
;; (set-face-attribute 'default nil :font "Envy Code R-10")
;; (set-face-attribute 'default nil :font "mononoki-10")
;; (set-face-attribute 'default nil :font "Hasklig-10")
;; (set-face-attribute 'default nil :font "consolas-10")
(set-face-attribute 'default nil :font "dejavu sans mono-10")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)  ;; key an eye on this, might cause problems
(setq show-paren-delay 0)
(show-paren-mode t)
(setq-default c-basic-offset 2)
(setq-default case-fold-search t)
(setq case-fold-search t)
(setq-default fill-column 70)
(setq kill-whole-line t)
(setq-default truncate-lines t)
(setq x-select-enable-clipboard t)  ;; Allows you to copy into the system clipboard.
(setq-default isearch-lazy-highlight-initial-delay 0)
; (global-git-gutter-mode t)
(setq-default sgml-basic-offset 2)

;;; Global key bindings
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-SPC") 'goto-line)
(global-set-key (kbd "M-F") 'find-file-at-point)
(global-set-key (kbd "C-`") (lambda () (interactive) (insert "`")))
(global-set-key (kbd "C-x m") 'bookmark-set)
(global-set-key (kbd "C-x l") 'bookmark-bmenu-list)
(global-set-key (kbd "M-i") (lambda () (interactive) (indent-rigidly (region-beginning) (region-end) 1)))
(global-set-key (kbd "M-I") (lambda () (interactive) (indent-rigidly (region-beginning) (region-end) -1)))
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "C-c C-o") 'find-file-at-point)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x o") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-R") 'query-replace-regexp)
(global-set-key (kbd "M-_") 'delete-indentation)
(global-set-key (kbd "M-T") 'transpose-lines)
(global-set-key (kbd "C-x t") 'transpose-lines)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-N") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "s-n") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "s-p") (lambda () (interactive) (other-window -1)))
(global-set-key [C-tab] (lambda () (interactive) (other-frame 1)))
(global-set-key [C-S-iso-lefttab] (lambda () (interactive) (other-frame -1)))
;; (global-set-key (kbd "C-x o") 'other-frame)
(global-set-key [\S-insert] 'clipboard-yank)
(global-set-key (kbd "C-o") 'open-start-line)
(global-set-key "\C-\\" 'match-paren)
(global-set-key (kbd "M-D") 'duplicate-line)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-,") 'switch-to-other-buffer)
;; (global-set-key (kbd "M-o") 'anything)
(global-set-key (kbd "M-o") 'neotree-toggle)
(global-set-key (kbd "M-@") 'kmacro-end-and-call-macro)
(global-set-key (kbd "M-.") 'mark-current-word)
(global-set-key (kbd "s-r") 'rename-buffer)
(global-set-key (kbd "C-x g r") 'insert-register)
(global-set-key (kbd "s-w") 'copy-to-register)
(global-set-key (kbd "s-y") 'insert-register)
(global-set-key (kbd "s-m") 'point-to-register)
(global-set-key (kbd "s-j") 'jump-to-register)
(global-set-key (kbd "s-s") 'svn-status)
(global-set-key (kbd "s-a") 'align-regexp)
(global-set-key (kbd "s-g") 'rgrep)
(global-set-key (kbd "s-i") 'indent-rigidly)
(global-set-key (kbd "s-t") 'string-rectangle)
(global-set-key (kbd "`") 'iswitchb-buffer)
(global-set-key (kbd "M-`") 'magit-status)

;; (global-set-key [C-return]
;;                 (lambda ()
;;                   (interactive)
;;                   (cua-selection-mode t)
;;                   (cua-set-rectangle-mark)))

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun comment-line ()
  (interactive)
  (point-to-register 'Z)
  (back-to-indentation)
  (set-mark-command nil)
  (move-end-of-line nil)
  (comment-dwim nil)
  (register-to-point 'Z)
  (pop-mark))

(defun duplicate-line ()
  (interactive)
  (let (
        ;; (line (line-number-at-pos nil))
        (col (current-column)))
    (move-beginning-of-line nil)
    (kill-line nil)
    (yank nil)
    ;; (newline)
    (yank nil)
    (previous-line 1)
    ;; (goto-line (- line 1))
    (move-to-column col)))

(defun mark-current-word ()
  "Put point at beginning of current word, set mark at end."
  (interactive)
  (let* ((opoint (point))
         (word (current-word))
         (word-length (length word)))
    (if (save-excursion
          ;; Avoid signaling error when moving beyond buffer.
          (if (> (point-min)  (- (point) word-length))
              (beginning-of-buffer)
            (forward-char (- (length word))))
          (search-forward word (+ opoint (length word))
                          'noerror))
        (progn (push-mark (match-end 0) nil t)
               (goto-char (match-beginning 0)))
      (error "No word at point" word))))

(defalias 'ff 'find-file)

;; (highlight-tags)
(defun match-paren (arg)
  "Go to the matching paren if on a paren."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

(defun open-start-line ()
  (interactive)
  (move-beginning-of-line 1)
  (open-line 1)
  (indent-according-to-mode))

(add-hook 'less-css-mode-hook
  (lambda ()
    (setq css-indent-offset 2)))

(add-hook 'css-mode-hook
  (lambda ()
    ;; (local-set-key (kbd "C-j")
    ;;   (lambda ()
    ;;     (interactive)
    ;;     (setq cssm-indent-level 2)
    ;;     (newline-and-indent)))
    (setq cssm-indent-level 2)
    ;; (wrap-region-mode t)
    (rainbow-mode t)
    (setq css-indent-offset 2)))

(add-hook 'html-mode-hook
  (lambda ()
    ;; (rainbow-mode t)
    ;; (tidy-build-menu html-mode-map)
    (local-set-key (kbd "C-c C-c") 'tidy-buffer)
    (local-set-key (kbd "M-n") 'sgml-skip-tag-forward)
    (local-set-key (kbd "M-p") 'sgml-skip-tag-backward)
    (setq sgml-validate-command "tidy")
    ;; (sgml-guess-indent)
    (setq sgml-basic-offset 2)
    ;; (wrap-region-mode t)
    ))

; (add-hook 'cua-selection-mode-hook
          ; (lambda ()
            ; (local-set-key (kbd "C-g")
                           ; (lambda ()
                             ; (keyboard-quit)
                             ; (cua-selection-mode nil)))))

(add-hook 'js-mode-hook
  (lambda ()
    ;; (local-set-key (kbd "C-j")
    ;;   (lambda () (interactive) (newline)))
    (local-set-key (kbd "C-j")
      (lambda () (interactive) (newline-and-indent)))
    (auto-complete-mode t)
    (setq js-indent-level 2)
    (yas-minor-mode t)
    (electric-indent-mode nil)
    ;; (flycheck-mode t)
    (tern-mode t)
    ;; (eval-after-load 'tern
    ;;   '(progn
    ;;     (require 'tern-auto-complete)
    ;;       (tern-ac-setup)))
    ;; (paredit-mode t)
    ;; (autopair-mode t)
    ;; (wrap-region-mode t)
    ))

(add-hook 'org-mode-hook
  (lambda ()
    (local-set-key (kbd "C-,") 'switch-to-other-buffer)
    (local-set-key (kbd "C-o") 'open-line)))

(add-hook 'paredit-mode-hook
  (lambda () (local-set-key (kbd "C-w") 'paredit-backward-kill-word)))

(add-hook 'python-mode-hook
  (lambda ()
    (setq python-indent 2)
    ;; (pysmell-mode 1)
    ;; (wrap-region-mode t)
    ))

(add-hook 'fundamental-mode-hook
  (lambda () (local-set-key (kbd "C-o") 'open-line)))

(add-hook 'shell-mode-hook
  (ansi-color-for-comint-mode-on))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Auto settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "4031c1ea0bb235b75a048bd92f3bf3aa984c9f7cc5b408f00f62ed99a6eecc09" default)))
 '(quack-fontify-style (quote emacs))
 '(quack-programs (quote ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values (quote ((name . box) (name . list) (name . controls) (name . reset) (name . __here__) (name . screen))))
 '(scss-compile-at-save nil)
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t nil))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; TEMP

;; (global-set-key (kbd "C-c C-c")
;;   (lambda ()
;;     (interactive)
;;     (mark-current-word)
;;     (clipboard-kill-ring-save (region-beginning) (region-end))))

;; (global-set-key (kbd "M-.") 'clipboard-kill-ring-save)
