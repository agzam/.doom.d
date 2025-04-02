;;; custom/learning/config.el -*- lexical-binding: t; -*-

(use-package! exercism
  :defer t
  :commands (exercism)
  :init
  (shell-command (format "%s configure --workspace ~/GitHub/agzam/exercism/"
                         (or (executable-find "exercism")
                             (user-error "exercism CLI not found"))))
  :config
  (setopt exercism--workspace (expand-file-name "~/GitHub/agzam/exercism/")))
