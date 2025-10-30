;;; custom/tab-bar/config.el -*- lexical-binding: t; -*-

(after! tab-bar
  (setq tab-bar-show t
        tab-bar-new-tab-group nil
        tab-bar-close-button-show nil
        tab-bar-separator " ❘ "
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
        tab-bar-auto-width t
        tab-bar-auto-width-max '((150) 10))

  (add-hook! 'tab-bar-tab-name-format-functions
             #'+tab-bar-fmt-show-index-fn)
  (remove-hook!
    'tab-bar-tab-name-format-functions
    #'tab-bar-tab-name-format-hints)

  (tab-bar-history-mode +1)
  (unless (featurep :system 'macos)
    (setopt tab-bar-tab-name-function #'+tab-bar-name-fn))

  (map!
   "s-[" #'tab-bar-switch-to-prev-tab
   "s-]" #'tab-bar-switch-to-next-tab
   "s-j" #'tab-bar-switch-to-prev-tab
   "s-k" #'tab-bar-switch-to-next-tab)

  ;; (add-hook! 'tab-bar-mode-hook #'reset-frame-full-height)

  (when (modulep! :custom completion)
    (add-hook! 'tab-bar-mode-hook #'+corfu-kill-frames))

  ;; tabs sometimes dissappear from the frame
  ;; I need to make sure they are on when tab switching
  (defadvice! tab-bar-switch-a (&optional arg)
    :before #'tab-bar-switch-to-prev-tab
    :before #'tab-bar-switch-to-next-tab
    :before #'tab-bar-transient
    (set-frame-parameter nil 'tab-bar-lines 1)))

(add-hook! 'doom-after-init-hook
  (defun init-desktop-mode-h ()
    (setopt desktop-path (list doom-profile-state-dir))
    (ignore-errors (make-directory doom-profile-state-dir))
    (desktop-save-mode 1)))

(setopt desktop-save t)

;; (add-hook! 'tab-bar-tab-added-hook #'tab-bar-created-h)
