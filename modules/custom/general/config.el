;;; common/general/config.el -*- lexical-binding: t; -*-

(use-package! embark-consult
  :config)

;; (use-package! prescient
;;   :config)

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

;; (setq-local vertico-extensions-path (concat straight-base-dir "straight/" straight-build-dir "/vertico/extensions"))

(use-package! vertico-repeat
  :after vertico
  :load-path "~/.emacs-profiles/.emacs-doom.d/.local/straight/build-28.0.60/vertico/extensions/"
  ;; (format "%s" straight-base-dir "straight/" straight-build-dir "/vertico/extensions")
  :config
  (map! :leader "rl" #'vertico-repeat))

(after! vertico
  (map! :map vertico-map
        "C-e" #'vertico-scroll-up
        "C-y" #'vertico-scroll-down

        ;; Doom's vertico module screws up the backspace
        [backspace] nil)

  (remove-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
