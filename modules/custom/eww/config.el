;;; custom/eww/config.el -*- lexical-binding: t; -*-

(use-package! eww
  :commands (eww)
  :config
  (setq shr-use-fonts nil
        shr-max-image-proportion 0.5)

  (map! :map eww-mode-map
        :ni "C-<return>" #'eww-open-in-other-window
        :n "yy" #'eww-copy-page-url
        (:localleader
         "z" #'eww-zoom-transient
         "F" #'eww-toggle-fonts
         :desc "copy url" "y" #'eww-copy-current-url+)))
