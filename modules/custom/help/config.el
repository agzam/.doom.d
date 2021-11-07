;;; custom/help/config.el -*- lexical-binding: t; -*-

(use-package! helpful
  :config
  (map! :leader
        :n "hh" #'helpful-symbol
        :n "hdd" #'helpful-symbol
        :n "ha" #'helpful-at-point)

  (after! elisp-mode
    (map! :localleader
          :map emacs-lisp-mode-map
          :prefix "h"
          "h" #'helpful-at-point))

  (global-set-key (kbd "C-h k") #'helpful-key)

  ;; ensure that browsing in Helpful and Info modes doesn't create additional
  ;; window splits
  (add-to-list
   'display-buffer-alist
   `(,(rx bos (or "*helpful" "*info"))
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.3))))

(use-package! info+
  :config
  (map! :leader "hj" #'info-display-manual)
  (setq Info-fontify-angle-bracketed-flag nil)
  (add-hook 'Info-mode-hook (lambda () (require 'info+))))
