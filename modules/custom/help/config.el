;;; custom/help/config.el -*- lexical-binding: t; -*-

(use-package! helpful
  :config
  (map! :leader
        "hh" #'helpful-symbol
        "hf" #'helpful-function
        "hv" #'helpful-variable
        "ha" #'helpful-at-point
        "hdd" nil ; muscle memory is still strong
        )

  (after! emacs-lisp-mode
    (map! :localleader
          :map emacs-lisp-mode-map
          "hh" #'helpful-at-point))

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
