;; Packages


(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-y" . claude-code-ide-menu) ; Set your favorite keybinding
  :config (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools
