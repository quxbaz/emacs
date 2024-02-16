;; Theme, appearance, typography, colors


;; Set theme and font.
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font "Monaco-10")

;; Disables the current theme before loading the next one to avoid overlapping
;; styles. Don't use this to set the font. Use load-theme instead.
(defun my/switch-theme (theme)
  (disable-theme (car custom-enabled-themes))
  (load-theme theme t))

;; Colors
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

(setq org-todo-keyword-faces '(("IN-PROGRESS" . "magenta")
                               ("WAITING" . "orange")
                               ("DONE" . "green")))

;; Favorites
;; (my/switch-theme 'doom-oceanic-next t)
;; (my/switch-theme 'modus-operandi-tinted t)
;; Keepers
;; - Dark (dark background, high contrast)
;; (my/switch-theme 'doom-challenger-deep)
;; (my/switch-theme 'doom-dracula)
;; (my/switch-theme 'doom-henna)
;; (my/switch-theme 'doom-monokai-pro)
;; (my/switch-theme 'doom-moonlight)
;; (my/switch-theme 'doom-oceanic-next)
;; (my/switch-theme 'doom-one)
;; (my/switch-theme 'doom-outrun-electric)
;; (my/switch-theme 'doom-snazzy)
;; (my/switch-theme 'doom-vibrant)
;; (my/switch-theme 'modus-operandi)
;; (my/switch-theme 'modus-operandi-deuteranopia)
;; (my/switch-theme 'modus-operandi-tinted)
;; (my/switch-theme 'modus-operandi-tritanopia)
;; - Neutral (neutral background, medium contrast)
;; (my/switch-theme 'doom-ephemeral)
;; (my/switch-theme 'doom-gruvbox)
;; (my/switch-theme 'doom-material)
;; (my/switch-theme 'doom-monokai-classic)
;; (my/switch-theme 'doom-nord)
;; (my/switch-theme 'doom-opera)
;; (my/switch-theme 'doom-palenight)
;; (my/switch-theme 'doom-peacock)
;; (my/switch-theme 'doom-wilmersdorf)
;; (my/switch-theme 'doom-zenburn)
;; - Light (light background, high contrast)
;; (my/switch-theme 'doom-acario-light)
;; (my/switch-theme 'doom-nord-light)
;; (my/switch-theme 'doom-one-light)
;; (my/switch-theme 'doom-tomorrow-day)
;; (my/switch-theme 'modus-vivendi)
;; (my/switch-theme 'modus-vivendi-deuteranopia)
;; (my/switch-theme 'modus-vivendi-tinted)
;; (my/switch-theme 'modus-vivendi-tritanopia)

;; Fonts
;; (set-face-attribute 'default nil :font "CascadiaCode-10")      ;; Nice, but small.
;; (set-face-attribute 'default nil :font "ComicMono-11")         ;; Bad. Squished. Cartoony.
;; (set-face-attribute 'default nil :font "DejaVuSansMono-10")    ;; Good default font.
;; (set-face-attribute 'default nil :font "FiraCode-10")          ;; Nice. Small, but legible.
;; (set-face-attribute 'default nil :font "Hack-10")              ;; Nice. Balanced.
;; (set-face-attribute 'default nil :font "Inconsolata-11")       ;; Not bad. Needs more line height.
;; (set-face-attribute 'default nil :font "Iosevka-10")           ;; Very skinny. Hard to read.
;; (set-face-attribute 'default nil :font "IosevkaComfyFixed-10") ;; A little nicer to look at.
;; (set-face-attribute 'default nil :font "JetBrainsMono-9")      ;; Kinda quirky.
;; (set-face-attribute 'default nil :font "Monaco-10")            ;; Stylish.
;; (set-face-attribute 'default nil :font "Mononoki-10")          ;; Too condensed.
;; (set-face-attribute 'default nil :font "NotoSansMono-10")      ;; Good.
;; (set-face-attribute 'default nil :font "SFMono-10")            ;; Very Cool. Love it.
;; (set-face-attribute 'default nil :font "SourceCodePro-10")     ;; Excellent.
;; (set-face-attribute 'default nil :font "Terminus-12")          ;; Actually not bad.
;; (set-face-attribute 'default nil :font "UbuntuMono-12")        ;; Weird.

;; Alternate ways of setting font.
;; (set-face-attribute 'default nil :font "SourceCodePro-10:pixelsize=14")
;; (set-frame-font "SourceCodePro-10:pixelsize=14")

;; Typography
;; (setq-default line-spacing nil)

;; Row background color in hl-line-mode.
;; (set-face-background 'hl-line "#404080")
;; (set-face-background 'hl-line "#0000a0")
