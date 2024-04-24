;;; custom/chat/config.el -*- lexical-binding: t; -*-

(use-package! telega
  :defer t
  :hook (telega-chat . jinx-mode)
  :config
  (when (featurep :system 'macos)
    (setq telega-server-libs-prefix "/opt/homebrew/opt/tdlib"))

  (setq telega-completing-read-function 'completing-read-default
        ;; telega-use-docker t
        )

  (map! :map telega-root-mode-map [remap imenu] #'telega-chat-with)

  (map! :map telega-msg-button-map "SPC" nil)

  (add-hook! 'telega-chat-update-hook
    (defun lg-telega-chat-update-h (_)
      (with-telega-root-buffer
       (hl-line-highlight))))

  ;; (add-hook! 'telega-chat-mode-hook
  ;;   (defun telega-chat-mode-h ()
  ;;     (setq line-spacing 9)))

  (add-hook! 'telega-root-mode-hook
    (defun lg-telega-root-mode-h ()
      (hl-line-mode 1)
      (setq line-spacing 9))))

