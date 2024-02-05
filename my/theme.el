;; Theme, appearance, typography, colors


;; Theme
(load-theme 'doom-dracula t)
;; (load-theme 'doom-material t)
;; (load-theme 'doom-monokai-pro t)
;; (load-theme 'doom-oceanic-next t)
;; (load-theme 'doom-tomorrow-day t)


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
(set-face-attribute 'default nil :font "Monaco-10")            ;; Stylish.
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


;; Colors
(custom-set-faces
 `(default ((t (:background "#00242a"))))
 '(font-lock-comment-face ((t (:foreground "#999"))))
 '(magit-section-highlight ((t (:inherit hl-line :background "blue")))))
