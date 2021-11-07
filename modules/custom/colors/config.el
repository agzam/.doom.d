;;; custom/colors/config.el -*- lexical-binding: t; -*-
(use-package! ag-themes)

(use-package! circadian
  :after (ag-themes)
  :config
  (setq
   ;; North of TX
   calendar-latitude 33.16
   calendar-longitude -96.93
   circadian-themes '(("7:00" . ag-themes-spacemacs-light)
                      ("18:30"  . ag-themes-base16-ocean)))
  (add-hook 'window-setup-hook #'circadian-setup))

(use-package! rainbow-mode)

(defun colors/cycle-themes-down ()
  (interactive)
  (colors/load-next-theme)
  (colors/cycle-themes/body))

(defun colors/cycle-themes-up ()
  (interactive)
  (colors/load-prev-theme)
  (colors/cycle-themes/body))

(map! :leader
      :n "Tn" #'colors/cycle-themes-down
      :n "Tp" #'colors/cycle-themes-up)
