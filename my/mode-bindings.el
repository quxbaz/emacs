;; Mode bindings
;;
;; The my/bind / my/apply-when helpers live in my/util.el.


(my/bind :now help-mode-map
  "<mouse-8>" 'help-go-back
  "<mouse-9>" 'help-go-forward)

(my/bind :now minibuffer-mode-map
  "M-/" 'dabbrev-expand)

(my/bind :now occur-mode-map
  "n" (my/cmd (occur-next) (occur-mode-display-occurrence))
  "p" (my/cmd (occur-prev) (occur-mode-display-occurrence)))

(my/bind :now text-mode-map
  "M-/" 'dabbrev-expand)

(my/bind (:hook bookmark-bmenu-mode-hook) bookmark-bmenu-mode-map
  "D" (my/cmd (bookmark-bmenu-unmark-all)
              (bookmark-bmenu-delete)
              (bookmark-bmenu-execute-deletions)))

(my/bind (:after calendar) calendar-mode-map
  "s-d"   'calendar-exit
  "<f12>" 'calendar-exit
  "l"     'calendar-forward-day
  "h"     'calendar-backward-day
  "j"     'calendar-forward-week
  "k"     'calendar-backward-week)

(my/bind (:hook css-mode-hook) css-mode-map
  "M-q" 'my/indent-block)

(my/bind (:after diff-mode) diff-mode-map
  "C-c C-c" 'diff-apply-hunk)

(my/bind (:after deadgrep) deadgrep-mode-map
  "C-o" 'my/deadgrep-display-result
  "N"   'my/deadgrep-display-next-result
  "P"   'my/deadgrep-display-prev-result)

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
  "C-r"      (my/cmd-or 'my/isearch-backward-dwim 'my/dired-resize-image))

;; Buffer-local map: wdired rebinds C-a / M-m to skip dired's permission columns.
(add-hook 'wdired-mode-hook
          (lambda ()
            (local-set-key (kbd "C-a") (lambda () (interactive) (move-to-column 2)))
            (local-set-key (kbd "M-m") (lambda () (interactive) (move-to-column 2)))))

(my/bind (:after ibuffer) ibuffer-mode-map
  "D"   'my/ibuffer-delete-no-conf
  "M-r" 'ibuffer-do-query-replace-regexp
  "/ ?" 'ibuffer-filter-by-predicate
  "/ e" 'ibuffer-filter-by-file-extension
  "/ q" 'ibuffer-pop-filter)

;; (local-set-key (kbd "M-.") 'lsp-find-definition)
;; (local-set-key (kbd "M-,") 'lsp-find-implementation)
;; (local-set-key (kbd "M-'") 'lsp-rename)
(my/bind (:after js) js-mode-map
  "M-q"     'my/indent-block
  "M-."     'my/mark-context
  "C-c C-/" 'my/toggle-jsx-comment
  "C-c /"   'my/close-html-tag)

(my/bind (:after magit) magit-mode-map
  "w"   'my/key-w
  "C-o" 'magit-diff-visit-file-other-window
  "G"   'my/magit-quick-commit)

(my/bind (:after git-commit) git-commit-mode-map
  "C-<return>" 'with-editor-finish)

(add-hook 'mathjax-mode-hook
          (lambda ()
            (yas-activate-extra-mode 'mathjax-mode)))

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

;; Buffer-local map: give sh-mode sentence motion on M-a / M-e.
(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local my/sh-mode-map (make-sparse-keymap))
            (define-key my/sh-mode-map (kbd "M-a") 'backward-sentence)
            (define-key my/sh-mode-map (kbd "M-e") 'forward-sentence)
            (use-local-map my/sh-mode-map)))

(my/bind (:hook web-mode-hook) web-mode-map
  "M-/"     'hippie-expand
  "C-M-i"   'completion-at-point
  "M-q"     'my/indent-block
  "C-c /"   (kmacro "< > C-b / C-f")
  "C-c C-c" 'my/eval-php)
