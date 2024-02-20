;; Theme, appearance, typography, colors


;; # Set theme and font.
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font "Monaco-10")

;; # Colors
(custom-set-faces
 ;; `(default ((t (:background "#00242a"))))
 ;; '(font-lock-comment-face ((t (:foreground "#999"))))
 ;; For certain themes which need a higher contrast line-nubmer.
 ;; '(line-number ((t (:inherit default :foreground "#999999" :strike-through nil :underline nil :slant normal :weight normal))))
 '(magit-section-highlight ((t (:inherit hl-line :background "blue")))))

(custom-set-variables
 '(prism-colors '("#1fceff" "#ff6bc4" "#ffc738" "#f95624"))
 '(prism-desaturations '(10 15 20))
 '(prism-lightens '(0 5 10))
 '(prism-comments nil)
 '(prism-strings nil)
 '(prism-level-1-strings ((t (:foreground "#999")))))

(setq org-todo-keyword-faces '(("IN-PROGRESS" . "yellow")
                               ("WAITING" . "orange")))


;; # Defuns
;; Disables the current theme before loading the next one to avoid overlapping
;; styles. Don't use this to set the font. Use load-theme instead.
(defun my/switch-theme (theme)
  (disable-theme (car custom-enabled-themes))
  (load-theme theme t))
