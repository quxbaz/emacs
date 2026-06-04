;; Theme, appearance, typography, colors


;; # Set theme and font.
(load-theme 'modus-vivendi t)
;; (set-face-attribute 'default nil :font "Monaco-10")
;; Set the font via default-frame-alist so it applies to every frame,
;; including ones the daemon creates for emacsclient (set-face-attribute at
;; init time doesn't reliably reach daemon-spawned frames).
(add-to-list 'default-frame-alist '(font . "Monaco-10:pixelsize=14"))
(set-face-attribute 'default nil :font "Monaco-11:pixelsize=15")


;; # Colors
;; ## mode line
(set-face-attribute 'mode-line  ;; 'mode-line-inactive also configurable.
                    nil
                    :foreground "white"
                    :background "violet red"
                    :box '(:line-width 1 :style released-button))

;; ## magit
(custom-set-faces '(magit-section-highlight ((t (:inherit hl-line :background "blue")))))


;; # Defuns
;; Disables the current theme before loading the next one to avoid overlapping
;; styles. Don't use this to set the font. Use load-theme instead.
(defun my/switch-theme (theme)
  (disable-theme (car custom-enabled-themes))
  (load-theme theme t))
