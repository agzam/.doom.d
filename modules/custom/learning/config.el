;;; custom/learning/config.el -*- lexical-binding: t; -*-

(use-package! exercism
  :defer t
  :commands (exercism)
  :init
  (if-let* ((exe (executable-find "exercism")))
    (shell-command (format "%s configure --workspace ~/GitHub/agzam/exercism/" exe))
    (print "exercism CLI not found"))
  :config
  (setopt exercism--workspace (expand-file-name "~/GitHub/agzam/exercism/")))
