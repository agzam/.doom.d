;;; custom/colors/config.el -*- lexical-binding: t; -*-

(use-package! ag-themes
  :after-call doom-init-ui-h
  :config)

(use-package! circadian
  :hook (window-setup . circadian-setup)
  :config
  (setq
   ;; North of TX
   ;; calendar-latitude 33.16
   ;; calendar-longitude -96.93
   circadian-themes '(("06:00" . ag-themes-spacemacs-light)
                      ;; ("20:00" . ag-themes-base16-ocean)
                      )))

(use-package! rainbow-mode
  :defer t)

(use-package! beacon
  :after-call doom-first-file-hook
  :config
  (setq beacon-blink-delay 0.1
        beacon-blink-duration 0.5
        beacon-size 60
        beacon-color "DarkGoldenrod2"
        beacon-blink-when-window-scrolls nil)
  (beacon-mode +1))
