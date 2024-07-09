;; Theme, appearance, typography, colors


;; # Set theme and font.
(load-theme 'modus-vivendi t)
;; (set-face-attribute 'default nil :font "Monaco-10")


;; # Colors
;; ## magit
(custom-set-faces '(magit-section-highlight ((t (:inherit hl-line :background "blue")))))
;; ## org-mode
(setq org-todo-keyword-faces '(("TODO" . "yellow")
                               ("IN-PROGRESS" . "cyan1")
                               ("WAITING" . "orange")
                               ("FAILED" . "DeepPink")
                               ("QUESTION" . "grey50")
                               ("REVIEW" . "orchid")
                               ("BACKLOG" . "grey50")
                               ("NOTE" . "grey50")
                               ("DONE" . "green")))


;; # Defuns
;; Disables the current theme before loading the next one to avoid overlapping
;; styles. Don't use this to set the font. Use load-theme instead.
(defun my/switch-theme (theme)
  (disable-theme (car custom-enabled-themes))
  (load-theme theme t))
