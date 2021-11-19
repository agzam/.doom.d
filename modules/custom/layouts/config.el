;;; custom/layouts/config.el -*- lexical-binding: t; -*-

(after! tab-bar
  (setq tab-bar-show t
        tab-bar-new-tab-group nil
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator)
        tab-bar-tab-name-function #'+tab-bar-name-fn)

  (map!
   "s-[" #'tab-bar-switch-to-prev-tab
   "s-]" #'tab-bar-switch-to-next-tab
   "s-j" #'tab-bar-switch-to-prev-tab
   "s-k" #'tab-bar-switch-to-next-tab)

  (map! :leader "l" #'+hydra/layouts/body)

  ;; (add-hook! (tab-bar-tab-added tab-bar-tab-removed) #'reset-frame-full-height)
  )
