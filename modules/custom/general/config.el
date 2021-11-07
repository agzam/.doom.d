;;; common/general/config.el -*- lexical-binding: t; -*-

(use-package! embark-consult
  :config)

(use-package! prescient
  :config)

(use-package! fasd
  :config
  (map! :leader "fad" #'fasd-find-file)
  (global-fasd-mode +1))

(use-package! consult-company
  :config
  (map! :map company [remap completion-at-point] #'consult-company))

(use-package! vertico-posframe
  :config
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (vertico-posframe-mode +1)
  (setq marginalia-margin-threshold 300))

(after! vertico
  (remove-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
