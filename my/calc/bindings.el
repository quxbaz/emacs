;; -*- lexical-binding: t; -*-
;;
;; Calc keybindings


;; calc-mode-map (early bindings)
(eval-after-load 'calc
  '(progn
     (keymap-set calc-mode-map "M-w" 'my/calc-kill-ring-save-dwim)
     (keymap-set calc-mode-map "SPC" 'my/calc-edit)))

;; calc-digit-map (minibuffer input)
(with-eval-after-load 'calc
  (define-key calc-digit-map ";" 'my/calc-insert-colon)
  (define-key calc-digit-map "p" 'my/calc-insert-pi-from-minibuffer)
  (define-key calc-digit-map "P" 'my/calc-insert-pi-from-minibuffer)
  (define-key calc-digit-map "e" 'my/calc-equal-to-from-minibuffer)
  (define-key calc-digit-map "o" 'my/calc-mod-360-from-minibuffer))

;; calc-var-name-map (minibuffer input)
(with-eval-after-load 'calc-store
  (define-key calc-var-name-map ";" 'my/calc-insert-colon))

;; Stack bindings
(use-package calc-ext
  :defer t
  :bind (;; Unbind
         (:map calc-mode-map ("C-j" . ignore))
         (:map calc-mode-map ("J" . ignore))
         (:map calc-mode-map ("j m" . ignore))
         (:map calc-mode-map ("<escape> RET" . ignore))
         (:map calc-mode-map ("v x" . ignore))

         ;; Basic stack operations
         (:map calc-mode-map ("C-M-k" . calc-reset))
         (:map calc-mode-map ("C-/" . my/calc-undo))
         (:map calc-mode-map ("U" . my/calc-undo))
         (:map calc-mode-map ("D" . my/calc-redo))
         (:map calc-mode-map ("C-k" . my/calc-kill))
         (:map calc-mode-map ("C-a" . my/calc-beginning-of-expression))
         (:map calc-mode-map ("M-w" . my/calc-kill-ring-save-dwim))
         (:map calc-mode-map ("DEL" . my/calc-pop))
         (:map calc-mode-map ("RET" . my/calc-ret))
         (:map calc-mode-map ("C-<return>" . my/calc-duplicate-no-move))
         (:map calc-mode-map ("S-<return>" . my/calc-roll-to-top))
         (:map calc-mode-map ("C-M-<return>" . my/calc-duplicate-stack))
         (:map calc-mode-map ("s RET" . my/calc-store-stack))
         (:map calc-mode-map ("r RET" . my/calc-recall-stack))

         ;; Math operations
         (:map calc-mode-map ("M-=" . calc-evaluate))
         (:map calc-mode-map ("k k" . my/calc-evaluate))
         (:map calc-mode-map ("o" . calc-inv))
         (:map calc-mode-map ("\\" . calc-sqrt))
         (:map calc-mode-map ("W" . my/calc-square))

         ;; Display and modes
         (:map calc-mode-map (">" . calc-scroll-left))
         (:map calc-mode-map ("<" . calc-scroll-right))
         (:map calc-mode-map ("G" . my/calc-toggle-big-language))
         (:map calc-mode-map ("d ," . calc-group-digits))

         ;; Conversions
         (:map calc-mode-map ("l d" . my/calc-to-degrees))
         (:map calc-mode-map ("l r" . my/calc-to-radians))
         (:map calc-mode-map ("l l" . calc-float))
         (:map calc-mode-map ("l c" . calc-fraction))
         (:map calc-mode-map ("M-o" . my/calc-mod-360))

         ;; Algebraic operations
         (:map calc-mode-map ("@" . my/calc-no-simplify-mode))
         (:map calc-mode-map ("=" . calc-equal-to))
         (:map calc-mode-map ("e" . calc-equal-to))
         (:map calc-mode-map ("x" . calc-expand))
         (:map calc-mode-map ("a e" . calc-simplify))
         (:map calc-mode-map ("a s" . calc-simplify-extended))

         ;; Solving
         (:map calc-mode-map ("i" . calc-solve-for))
         (:map calc-mode-map ("a l" . calc-poly-roots))
         (:map calc-mode-map ("M-." . calc-remove-equal))
         (:map calc-mode-map ("a m" . calc-map-equation))
         (:map calc-mode-map ("a o" . calc-collect))
         (:map calc-mode-map ("l f" . my/calc-factor-by))

         ;; Variables
         (:map calc-mode-map ("," . my/calc-quick-variable))
         (:map calc-mode-map ("p" . my/calc-recall))
         (:map calc-mode-map ("s a" . calc-edit-variable))

         ;; Selection operations
         (:map calc-mode-map ("C-d" . calc-del-selection))
         (:map calc-mode-map ("j c" . my/calc-clear-selections))
         (:map calc-mode-map ("j i" . calc-sel-isolate))
         (:map calc-mode-map ("j e" . my/calc-sel-jump-equals))
         (:map calc-mode-map ("j l" . calc-commute-left))
         (:map calc-mode-map ("j r" . calc-commute-right))
         (:map calc-mode-map ("j j" . calc-select-more))
         (:map calc-mode-map ("j k" . calc-select-less))
         (:map calc-mode-map ("O" . my/calc-commute))
         (:map calc-mode-map ("N" . my/calc-sel-negate))
         (:map calc-mode-map ("j D" . my/calc-sel-distribute))
         (:map calc-mode-map ("j M" . my/calc-sel-merge))

         ;; Vectors and statistics
         (:map calc-mode-map ("(" . my/calc-vector-edit))
         (:map calc-mode-map ("M-h" . calc-head))
         (:map calc-mode-map ("v RET" . calc-index))
         (:map calc-mode-map ("F" . calc-reduce))
         (:map calc-mode-map ("k t" . calc-perm))
         (:map calc-mode-map ("M" . calc-map))
         (:map calc-mode-map ("u D" . calc-vector-median))
         (:map calc-mode-map ("v o" . calc-sort))
         (:map calc-mode-map ("M-u" . calc-unpack))

         ;; Rewrite rules
         (:map calc-mode-map ("X" . my/calc-log-exp-rules))
         (:map calc-mode-map ("k s" . my/calc-complete-the-square))
         (:map calc-mode-map ("k d" . my/calc-factor-powers))

         ;; Trail
         (:map calc-mode-map ("t o" . calc-trail-in))
         (:map calc-trail-mode-map ("t o" . calc-trail-out))))

;; Edit mode bindings
(add-hook 'calc-edit-mode-hook
          (lambda ()
            ;; Init
            (setq my/calc-history-index -1)

            ;; Config
            (push 34 (cl-getf autopair-dont-pair :never))  ;; Don't autopair double quotes.

            ;; Ignore
            (keymap-set calc-edit-mode-map "<escape> RET" 'ignore)

            ;; Basic editing
            (keymap-set calc-edit-mode-map "M-w" 'my/lisp-kill-ring-save-dwim)
            (keymap-set calc-edit-mode-map "C-M-." 'my/mark-list-command)
            (keymap-set calc-edit-mode-map "RET" 'my/calc-edit-finish)
            (keymap-set calc-edit-mode-map "S-<return>" 'my/calc-edit-newline)
            (keymap-set calc-edit-mode-map "M-<return>" 'my/calc-edit-duplicate)
            (keymap-set calc-edit-mode-map "C-<return>" 'my/calc-duplicate-paren-expr)

            ;; History
            (keymap-set calc-edit-mode-map "M-p" 'my/calc-edit-history-prev)
            (keymap-set calc-edit-mode-map "M-n" 'my/calc-edit-history-next)

            ;; Toggle operations
            (keymap-set calc-edit-mode-map "@" 'my/calc-no-simplify-mode)

            ;; Special characters and shortcuts
            (keymap-set calc-edit-mode-map ";" (my/cmd (insert ":")))
            (keymap-set calc-edit-mode-map "P" (my/cmd (insert "pi")))

            ;; Power shortcuts
            (keymap-set calc-edit-mode-map "M-2" (my/cmd (insert "^2")))
            (keymap-set calc-edit-mode-map "M-3" (my/cmd (insert "^3")))
            (keymap-set calc-edit-mode-map "M-4" (my/cmd (insert "^4")))
            (keymap-set calc-edit-mode-map "M-5" (my/cmd (insert "^5")))
            (keymap-set calc-edit-mode-map "M-6" (my/cmd (insert "^6")))
            (keymap-set calc-edit-mode-map "M-7" (my/cmd (insert "^7")))
            (keymap-set calc-edit-mode-map "M-8" (my/cmd (insert "^8")))
            (keymap-set calc-edit-mode-map "M-9" (my/cmd (insert "^9")))
            (keymap-set calc-edit-mode-map "M-'" (my/cmd (insert "^()") (backward-char)))

            ;; Math operations
            (keymap-set calc-edit-mode-map "'" 'my/calc-edit-square-dwim)
            (keymap-set calc-edit-mode-map "W" 'my/calc-edit-square-dwim)
            (keymap-set calc-edit-mode-map "\"" 'my/calc-edit-cube-dwim)
            (keymap-set calc-edit-mode-map "\\" 'my/calc-edit-sqrt-dwim)
            (keymap-set calc-edit-mode-map "L" 'my/calc-edit-ln)))

(provide 'my/calc/bindings)
