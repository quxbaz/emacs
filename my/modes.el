;; -*- lexical-binding: t; -*-
;;
;; Per-mode configuration.
;;
;; Each mode's whole configuration -- settings, hooks, and keybindings -- lives
;; in one `my/setup' form. Genuinely global configuration stays in my/conf.el;
;; the my/setup / my/bind / my/cmd helpers live in my/util.el.


(my/setup bookmark
  :init (setq bookmark-save-flag 1)  ;; Save ~/.emacs.d/bookmarks on every change.
  :bindings (:hook bookmark-bmenu-mode-hook) bookmark-bmenu-mode-map
            "D" (my/cmd (bookmark-bmenu-unmark-all)
                        (bookmark-bmenu-delete)
                        (bookmark-bmenu-execute-deletions)))


(my/setup calendar
  :hooks (calendar-mode-hook 'diary-mark-entries)
  :bindings (:after calendar) calendar-mode-map
            "s-d"   'calendar-exit
            "<f12>" 'calendar-exit
            "l"     'calendar-forward-day
            "h"     'calendar-backward-day
            "j"     'calendar-forward-week
            "k"     'calendar-backward-week)


(my/setup css
  :hooks (css-mode-hook (lambda () (setq css-indent-offset 2)))
  :bindings (:hook css-mode-hook) css-mode-map
            "M-q" 'my/indent-block)


(my/setup deadgrep
  :bindings (:after deadgrep) deadgrep-mode-map
            "C-o" 'my/deadgrep-display-result
            "N"   'my/deadgrep-display-next-result
            "P"   'my/deadgrep-display-prev-result)


(my/setup diff-mode
  :bindings (:after diff-mode) diff-mode-map
            "C-c C-c" 'diff-apply-hunk)


(my/setup dired
  :init (setq dired-dwim-target t)  ;; Use other dired window as default copy/move path.
        (setq wdired-allow-to-change-permissions t)  ;; Allow editing file modes.
        (setq dired-listing-switches "-laXGh --group-directories-first")  ;; Order directories first.
        (setq dired-omit-files "^\\.$\\|^\\.\\.$")  ;; Hide . and .. directories.
        (setq dired-omit-lines nil)
        (setq dired-omit-extensions nil)
        (advice-add 'dired-find-buffer-nocreate :override #'ignore)  ;; Always create a fresh dired buffer instead of reusing an existing one.
  :after dired
         (add-to-list 'dired-no-confirm 'load)  ;; L (dired-do-load) without confirmation.
  :hooks (dired-mode-hook (lambda ()
                            (dired-hide-details-mode t)
                            (dired-omit-mode t)
                            (put 'dired-find-alternate-file 'disabled nil)
                            (when (file-remote-p default-directory)
                              (setq dired-listing-switches "-l"))))
         ;; wdired (buffer-local map): rebind C-a / M-m to skip dired's permission columns.
         (wdired-mode-hook
          (lambda ()
            (local-set-key (kbd "C-a") (lambda () (interactive) (move-to-column 2)))
            (local-set-key (kbd "M-m") (lambda () (interactive) (move-to-column 2)))))
  :bindings (:hook dired-mode-hook) dired-mode-map
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


(my/setup edebug
  ;; edebug-eval-mode buffers don't default to a Lisp mode; switch them to
  ;; emacs-lisp-mode so they pick up the shared Lisp editing setup and bindings.
  :hooks (edebug-eval-mode-hook (lambda ()
                                  (unless (eq major-mode 'emacs-lisp-mode)
                                    (emacs-lisp-mode)))))


;; empty-line-mode is loaded/enabled in init.el; this sets its fringe bitmap
;; (see the palette in empty-line-mode.el).
(my/setup empty-line
  :init (setq empty-line-bitmap 'hollow-square))


(my/setup git-commit
  :bindings (:after git-commit) git-commit-mode-map
            "C-<return>" 'with-editor-finish)


(my/setup help
  :init (setq help-window-select t)  ;; Focus describe buffers on load.
  :bindings :now help-mode-map
            "<mouse-8>" 'help-go-back
            "<mouse-9>" 'help-go-forward)


(my/setup ibuffer
  :bindings (:after ibuffer) ibuffer-mode-map
            "D"   'my/ibuffer-delete-no-conf
            "M-r" 'ibuffer-do-query-replace-regexp
            "/ ?" 'ibuffer-filter-by-predicate
            "/ e" 'ibuffer-filter-by-file-extension
            "/ q" 'ibuffer-pop-filter)


(my/setup js
  :hooks (js-mode-hook (lambda ()
                         (setq js-indent-level 2)
                         (setq-local require-final-newline nil)))
  ;; (local-set-key (kbd "M-.") 'lsp-find-definition)
  ;; (local-set-key (kbd "M-,") 'lsp-find-implementation)
  ;; (local-set-key (kbd "M-'") 'lsp-rename)
  :bindings (:after js) js-mode-map
            "M-q"     'my/indent-block
            "M-."     'my/mark-context
            "C-c C-/" 'my/toggle-jsx-comment
            "C-c /"   'my/close-html-tag)


(my/setup lua
  :init (setq lua-indent-level 2))


(my/setup magit
  :init (setq magit-section-initial-visibility-alist
              '((recent . show)
                (unpushed . show)
                (untracked . show)
                (unstaged . show)        ;; Expand the Unstaged section (show the file list)...
                ([file unstaged] . hide)))  ;; ...but keep each file's diff collapsed.
  :hooks (magit-mode-hook (lambda ()
                            (setq magit-save-repository-buffers nil)
                            (add-to-list 'magit-no-confirm 'stage-all-changes)
                            (add-to-list 'magit-no-confirm 'unstage-all-changes)))
         ;; Keep diff-hl's gutter in sync with magit's staging/commit operations.
         (magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bindings (:after magit) magit-mode-map
            "w"   'my/key-w
            "C-o" 'magit-diff-visit-file-other-window
            "L"   'my/magit-load
            "G"   'my/magit-quick-commit)


(my/setup mathjax
  :init (define-minor-mode mathjax-mode
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
  :hooks (mathjax-mode-hook (lambda () (yas-activate-extra-mode 'mathjax-mode))))


(my/setup minibuffer
  :bindings :now minibuffer-mode-map
            "M-/" 'dabbrev-expand)


(my/setup occur
  :hooks (occur-mode-hook (lambda () (switch-to-buffer-other-window "*Occur*")))
  :bindings :now occur-mode-map
            "n" (my/cmd (occur-next) (occur-mode-display-occurrence))
            "p" (my/cmd (occur-prev) (occur-mode-display-occurrence)))


(my/setup org
  :init (setq org-adapt-indentation t)  ;; Indent after headings.
        (setq org-confirm-babel-evaluate nil)  ;; Evaluate code without confirmation.
        (custom-set-faces
         '(org-ellipsis ((t (:foreground "gray50")))))
        (setq org-ellipsis " [...]")
        (setq org-todo-keywords '((sequence "TODO"              "NEXT"                 "IN-PROGRESS"             "WAITING"              "FAILED"                "QUESTION"              "REVIEW"              "LIMBO"              "BACKLOG"      "|"     "NOTE"              "DONE-INT"                  "DONE")))
        (setq org-todo-keyword-faces     '(("TODO" . "yellow") ("NEXT" . "OrangeRed") ("IN-PROGRESS" . "cyan1") ("WAITING" . "orange") ("FAILED" . "DeepPink") ("QUESTION" . "grey50") ("REVIEW" . "orchid") ("LIMBO" . "grey50") ("BACKLOG" . "grey50") ("NOTE" . "grey50") ("DONE-INT" . "PaleGreen2") ("DONE" . "green")))
        (font-lock-add-keywords 'org-mode '(("`[^`\n]+`" 0 'org-code t)))  ;; Highlight `backtick` spans like =code=.
  :after org
         (set-face-attribute 'org-block-begin-line nil :foreground (face-foreground 'org-verbatim nil t))
         (set-face-attribute 'org-block-end-line nil :foreground (face-foreground 'org-verbatim nil t))
  :bindings (:after org) org-mode-map
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


;; prog-mode is the base mode for all programming modes.
(my/setup prog-mode
  :hooks (prog-mode-hook (lambda ()
                           (electric-pair-mode 0)
                           (diff-hl-mode t)
                           (hs-minor-mode t)
                           (show-point-mode t))))


(my/setup sh-mode
  ;; Buffer-local map: give sh-mode sentence motion on M-a / M-e.
  :hooks (sh-mode-hook
          (lambda ()
            (setq-local my/sh-mode-map (make-sparse-keymap))
            (define-key my/sh-mode-map (kbd "M-a") 'backward-sentence)
            (define-key my/sh-mode-map (kbd "M-e") 'forward-sentence)
            (use-local-map my/sh-mode-map))))


(my/setup sql
  :init (setq sql-connection-alist '((wnmu-edu-db (sql-product 'mysql)
                                                  (sql-user "david")
                                                  (sql-password "")
                                                  (sql-server "localhost")
                                                  (sql-database "wnmu_edu_db")))))


(my/setup text-mode
  :bindings :now text-mode-map
            "M-/" 'dabbrev-expand)


(my/setup web-mode
  :hooks (web-mode-hook (lambda ()
                          (setq web-mode-enable-auto-indentation nil)
                          (setq web-mode-markup-indent-offset 2)
                          (setq web-mode-code-indent-offset 2)
                          (setq web-mode-css-indent-offset 2)))
  :bindings (:hook web-mode-hook) web-mode-map
            "M-/"     'hippie-expand
            "C-M-i"   'completion-at-point
            "M-q"     'my/indent-block
            "C-c /"   (kmacro "< > C-b / C-f")
            "C-c C-c" 'my/eval-php)
