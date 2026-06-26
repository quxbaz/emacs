;; -*- lexical-binding: t; -*-
;;
;; Per-mode configuration.
;;
;; Each mode's settings, hook, and keybindings live together in one block, so a
;; mode's whole configuration is in one place. Genuinely global configuration
;; stays in my/conf.el; the my/bind / my/cmd / my/apply-when helpers live in
;; my/util.el.


;; # bookmark
(setq bookmark-save-flag 1)  ;; Save ~/.emacs.d/bookmarks on every change.
(my/bind (:hook bookmark-bmenu-mode-hook) bookmark-bmenu-mode-map
  "D" (my/cmd (bookmark-bmenu-unmark-all)
              (bookmark-bmenu-delete)
              (bookmark-bmenu-execute-deletions)))


;; # calendar
(add-hook 'calendar-mode-hook 'diary-mark-entries)
(my/bind (:after calendar) calendar-mode-map
  "s-d"   'calendar-exit
  "<f12>" 'calendar-exit
  "l"     'calendar-forward-day
  "h"     'calendar-backward-day
  "j"     'calendar-forward-week
  "k"     'calendar-backward-week)


;; # css
(add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))
(my/bind (:hook css-mode-hook) css-mode-map
  "M-q" 'my/indent-block)


;; # deadgrep
(my/bind (:after deadgrep) deadgrep-mode-map
  "C-o" 'my/deadgrep-display-result
  "N"   'my/deadgrep-display-next-result
  "P"   'my/deadgrep-display-prev-result)


;; # diff-mode
(my/bind (:after diff-mode) diff-mode-map
  "C-c C-c" 'diff-apply-hunk)


;; # dired
(setq dired-dwim-target t)  ;; Use other dired window as default copy/move path.
(setq wdired-allow-to-change-permissions t)  ;; Allow editing file modes.
(setq dired-listing-switches "-laXGh --group-directories-first")  ;; Order directories first.
(setq dired-omit-files "^\\.$\\|^\\.\\.$")  ;; Hide . and .. directories.
(setq dired-omit-lines nil)
(setq dired-omit-extensions nil)
(with-eval-after-load 'dired (add-to-list 'dired-no-confirm 'load))  ;; L (dired-do-load) without confirmation.
(advice-add 'dired-find-buffer-nocreate :override #'ignore)  ;; Always create a fresh dired buffer instead of reusing an existing one.

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
            (dired-omit-mode t)
            (put 'dired-find-alternate-file 'disabled nil)
            (when (file-remote-p default-directory)
              (setq dired-listing-switches "-l"))))

(my/bind (:hook dired-mode-hook) dired-mode-map
  "P"        'my/dired-up-directory
  "/"        'isearch-forward-regexp
  "M"        'dired-mark-files-regexp
  "C-c C-c"  'dired-toggle-read-only
  "r"        'my/dired-do-rename-here
  "+"        'dired-create-empty-file
  "C-+"      'dired-create-directory
  "<return>" 'my/dired-ret
  "I"        'dired-do-isearch-regexp
  "L"        'my/dired-do-load
  "M-r"      'dired-do-query-replace-regexp
  "F"        'my/find-regex-dired
  "N"        'my/find-name-dired
  "E"        'my/dired-context-edit-file
  "G"        'my/dired-open-gallery
  "q"        (my/cmd (quit-window t))  ;; Kill the dired buffer instead of burying it.
  "C-r"      (my/cmd-or 'my/isearch-backward-dwim 'my/dired-resize-image))

;; wdired (buffer-local map): rebind C-a / M-m to skip dired's permission columns.
(add-hook 'wdired-mode-hook
          (lambda ()
            (local-set-key (kbd "C-a") (lambda () (interactive) (move-to-column 2)))
            (local-set-key (kbd "M-m") (lambda () (interactive) (move-to-column 2)))))


;; # edebug
;; edebug-eval-mode buffers don't default to a Lisp mode; switch them to
;; emacs-lisp-mode so they pick up the shared Lisp editing setup and bindings.
(add-hook 'edebug-eval-mode-hook
          (lambda ()
            (unless (eq major-mode 'emacs-lisp-mode)
              (emacs-lisp-mode))))


;; # git-commit
(my/bind (:after git-commit) git-commit-mode-map
  "C-<return>" 'with-editor-finish)


;; # help
(setq help-window-select t)  ;; Focus describe buffers on load.
(my/bind :now help-mode-map
  "<mouse-8>" 'help-go-back
  "<mouse-9>" 'help-go-forward)


;; # ibuffer
(my/bind (:after ibuffer) ibuffer-mode-map
  "D"   'my/ibuffer-delete-no-conf
  "M-r" 'ibuffer-do-query-replace-regexp
  "/ ?" 'ibuffer-filter-by-predicate
  "/ e" 'ibuffer-filter-by-file-extension
  "/ q" 'ibuffer-pop-filter)


;; # js
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)
            (setq-local require-final-newline nil)))
;; (local-set-key (kbd "M-.") 'lsp-find-definition)
;; (local-set-key (kbd "M-,") 'lsp-find-implementation)
;; (local-set-key (kbd "M-'") 'lsp-rename)
(my/bind (:after js) js-mode-map
  "M-q"     'my/indent-block
  "M-."     'my/mark-context
  "C-c C-/" 'my/toggle-jsx-comment
  "C-c /"   'my/close-html-tag)


;; # lua
(setq lua-indent-level 2)


;; # magit
(setq magit-section-initial-visibility-alist
      '((recent . show)
        (unpushed . show)
        (untracked . show)
        (unstaged . show)        ;; Expand the Unstaged section (show the file list)...
        ([file unstaged] . hide)))  ;; ...but keep each file's diff collapsed.

(add-hook 'magit-mode-hook
          (lambda ()
            (setq magit-save-repository-buffers nil)
            (add-to-list 'magit-no-confirm 'stage-all-changes)
            (add-to-list 'magit-no-confirm 'unstage-all-changes)))

;; Keep diff-hl's gutter in sync with magit's staging/commit operations.
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(my/bind (:after magit) magit-mode-map
  "w"   'my/key-w
  "C-o" 'magit-diff-visit-file-other-window
  "G"   'my/magit-quick-commit)


;; # mathjax
(define-minor-mode mathjax-mode
  "Mode for writing MathJax snippets."
  ;; If t, the minor mode is enabled by default.
  :init-value nil
  ;; Mode Line text when the minor mode is active.
  :lighter " Mathjax"
  ;; Custom keybindings when the minor mode is active.
  :keymap (let ((map (make-sparse-keymap)))
            ;; (define-key map (kbd "KEY-SEQUENCE") 'COMMAND)
            map)
  ;; Specifies whether the mode is global (t) or buffer local (nil).
  :global nil
  ;;The customization group under which the mode settings are categorized.
  :group 'mathjax-mode)

(add-hook 'mathjax-mode-hook
          (lambda ()
            (yas-activate-extra-mode 'mathjax-mode)))


;; # minibuffer
(my/bind :now minibuffer-mode-map
  "M-/" 'dabbrev-expand)


;; # occur
(add-hook 'occur-mode-hook (lambda () (switch-to-buffer-other-window "*Occur*")))
(my/bind :now occur-mode-map
  "n" (my/cmd (occur-next) (occur-mode-display-occurrence))
  "p" (my/cmd (occur-prev) (occur-mode-display-occurrence)))


;; # org
(setq org-adapt-indentation t)  ;; Indent after headings.
(setq org-confirm-babel-evaluate nil)  ;; Evaluate code without confirmation.
(custom-set-faces
 '(org-ellipsis ((t (:foreground "gray50")))))
(setq org-ellipsis " [...]")
(setq org-todo-keywords '((sequence "TODO"              "NEXT"                 "IN-PROGRESS"             "WAITING"              "FAILED"                "QUESTION"              "REVIEW"              "LIMBO"              "BACKLOG"      "|"     "NOTE"              "DONE-INT"                  "DONE")))
(setq org-todo-keyword-faces     '(("TODO" . "yellow") ("NEXT" . "OrangeRed") ("IN-PROGRESS" . "cyan1") ("WAITING" . "orange") ("FAILED" . "DeepPink") ("QUESTION" . "grey50") ("REVIEW" . "orchid") ("LIMBO" . "grey50") ("BACKLOG" . "grey50") ("NOTE" . "grey50") ("DONE-INT" . "PaleGreen2") ("DONE" . "green")))
(font-lock-add-keywords 'org-mode '(("`[^`\n]+`" 0 'org-code t)))  ;; Highlight `backtick` spans like =code=.
(with-eval-after-load 'org
  (set-face-attribute 'org-block-begin-line nil :foreground (face-foreground 'org-verbatim nil t))
  (set-face-attribute 'org-block-end-line nil :foreground (face-foreground 'org-verbatim nil t)))

(my/bind (:after org) org-mode-map
  "M-a"        'org-backward-element
  "M-e"        'org-forward-element
  "M-p"        'org-previous-visible-heading
  "M-n"        'org-next-visible-heading
  ;; "M--" 'org-meta-return
  "M-j"        'org-meta-return
  "C-j"        'org-newline-and-indent
  "|"          'my/key-pipe
  "C-o"        'my/open-line
  "M-<return>" 'duplicate-dwim
  "M-q"        'fill-paragraph
  "C-,"        'my/switch-to-other-buffer
  "C-c C-v"    'my/revert-buffer
  "C-M--"      'org-insert-todo-heading
  "C-M-."      'my/org-table-mark-field
  "C-c C-o"    'my/org-open-at-point-dwim
  "C-c C-y"    'my/org-evaluate-time-range
  "C-C C-,"    'org-agenda
  "<f5>"       'org-html-export-to-html
  "C-c C-b"    'my/diff-buffer-with-file)


;; # prog-mode (base mode for all programming modes)
(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 0)
            (diff-hl-mode t)
            (hs-minor-mode t)
            (show-point-mode t)))


;; # sh-mode
;; Buffer-local map: give sh-mode sentence motion on M-a / M-e.
(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local my/sh-mode-map (make-sparse-keymap))
            (define-key my/sh-mode-map (kbd "M-a") 'backward-sentence)
            (define-key my/sh-mode-map (kbd "M-e") 'forward-sentence)
            (use-local-map my/sh-mode-map)))


;; # sql
(setq sql-connection-alist '((wnmu-edu-db (sql-product 'mysql)
                                          (sql-user "david")
                                          (sql-password "")
                                          (sql-server "localhost")
                                          (sql-database "wnmu_edu_db"))))


;; # text-mode
(my/bind :now text-mode-map
  "M-/" 'dabbrev-expand)


;; # web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-enable-auto-indentation nil)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-css-indent-offset 2)))
(my/bind (:hook web-mode-hook) web-mode-map
  "M-/"     'hippie-expand
  "C-M-i"   'completion-at-point
  "M-q"     'my/indent-block
  "C-c /"   (kmacro "< > C-b / C-f")
  "C-c C-c" 'my/eval-php)
