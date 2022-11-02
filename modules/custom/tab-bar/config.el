;;; custom/tab-bar/config.el -*- lexical-binding: t; -*-

(after! tab-bar
  (setq tab-bar-show t
        tab-bar-new-tab-group nil
        tab-bar-close-button-show nil
        tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator)
        tab-bar-tab-name-function #'+tab-bar-name-fn)

  (map!
   "s-[" #'tab-bar-switch-to-prev-tab
   "s-]" #'tab-bar-switch-to-next-tab
   "s-j" #'tab-bar-switch-to-prev-tab
   "s-k" #'tab-bar-switch-to-next-tab)

  (add-hook! 'tab-bar-mode-hook #'reset-frame-full-height)

  ;; tabs sometimes dissappear from the frame
  ;; I need to make sure they are on when tab switching
  (defadvice! tab-bar-switch-a (&optional arg)
    :before #'tab-bar-switch-to-prev-tab
    :before #'tab-bar-switch-to-next-tab
    :before #'tab-bar-transient
    (set-frame-parameter nil 'tab-bar-lines 1)))

;; (add-hook! 'tab-bar-tab-added-hook #'tab-bar-created-h)
