;;; custom/colors/config.el -*- lexical-binding: t; -*-

(use-package! circadian
  :config

  (setq
   ;; North of TX
   calendar-latitude 33.16
   calendar-longitude -96.93
   circadian-themes '((:sunrise . spacemacs-light)
                      (:sunset  . base16-ocean)))
  (circadian-setup)

  ;; (setq circadian-themes '(("6:00"  . spacemacs-light)
  ;;                          ("11.59" . tango)
  ;;                          ("16:00" . modus-vivendi)
  ;;                          ("19:00" . deeper-blue)
  ;;                          ("23:00" . base16-ocean)))
  )

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

