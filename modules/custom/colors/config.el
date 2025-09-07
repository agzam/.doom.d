;;; custom/colors/config.el -*- lexical-binding: t; -*-

(setopt pulse-delay 0.05)

(remove-hook! 'doom-init-ui-hook #'window-divider-mode)

(use-package! ag-themes
  :after-call doom-init-ui-h
  :config)

(use-package! circadian
  :defer t
  :hook (window-setup . circadian-setup)
  :config
  ;; North of TX
  (setq calendar-latitude 33.16
        calendar-longitude -96.93)
  (setf circadian-themes
        `(("6:00" . ag-themes-spacemacs-light)
          ("14:00" . ag-themes-ef-elea-light)
          ("20:00" . ag-themes-base16-ocean)
          ("21:30" . ag-themes-ef-elea-dark)
          ("23:00" . ag-themes-base16-ashes))))

(use-package! rainbow-mode
  :defer t)

(use-package! beacon
  :after-call (doom-first-file-hook)
  :config
  (setq beacon-blink-delay 0.1
        beacon-blink-duration 0.7
        beacon-size 60
        beacon-color "DarkGoldenrod2"
        beacon-blink-when-window-scrolls nil)
  (when (and (display-graphic-p)
             (not (featurep :system 'macos)))
   (beacon-mode +1)))

(use-package! ef-themes
  :config
  (setopt ef-themes-mixed-fonts nil
          ef-themes-variable-pitch-ui nil))
