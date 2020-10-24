(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'color-theme)
`
;; (add-to-list 'load-path "~/.emacs.d/rainbow-mode")
;; (require 'rainbow-mode)

;; (add-to-list 'load-path "~/.emacs.d/yasnippet")
;; (require 'yasnippet)
;; (yas-initialize)
(yas-load-directory "~/.emacs.d/snippets")

;; (add-to-list 'load-path "~/.emacs.d/el-get/wrap-region")
;; (require 'wrap-region)

;; (add-to-list 'load-path "~/.emacs.d/el-get/magit")
;; (require 'magit)

(require 'uniquify)
