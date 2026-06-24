;;; early-init.el --- Loaded before package init and the first frame -*- lexical-binding: t; -*-

;; early-init.el runs much earlier than init.el: before the package system is
;; brought up and before the initial GUI frame is created.  That makes it the
;; right place for anything that should take effect *before* the frame appears,
;; so the frame is born in its final shape with no flash or resize.

;; --- Garbage collection ---
;; Don't collect during startup (fewer pauses, faster init); restore a sane
;; threshold once startup is done.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)))

;; --- Packages ---
;; init.el calls `package-initialize' itself, so skip Emacs's implicit
;; pre-init initialization (otherwise it runs twice).
(setq package-enable-at-startup nil)

;; --- Initial frame ---
;; Set everything that shapes the initial frame BEFORE it is created, via
;; `default-frame-alist', so there is no flash of light, no GUI bars appearing
;; then vanishing, and no resize.  (Previously set after the frame in conf.el
;; and theme.el.)
(setq frame-inhibit-implied-resize t)            ; no resize for bars/font changes
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(push '(font . "Monaco-10:pixelsize=14") default-frame-alist)
;; Match the modus-vivendi colors so there is no white flash before the theme
;; loads in init.
(push '(background-color . "#000000") default-frame-alist)
(push '(foreground-color . "#ffffff") default-frame-alist)

;; --- Native compilation ---
;; Keep its noise out of the way.  Uncomment the second line to disable
;; just-in-time native compilation entirely (prevents recompiling bundled
;; *.eln / loaddefs at startup, at the cost of running byte/interpreted code).
(setq native-comp-async-report-warnings-errors 'silent)
;; (setq native-comp-jit-compilation nil)

;;; early-init.el ends here
