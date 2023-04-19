;;; custom/chat/config.el -*- lexical-binding: t; -*-

(use-package! telega
  :defer t
  :config
  (setq telega-server-libs-prefix "/opt/homebrew/opt/tdlib"
        telega-completing-read-function 'completing-read-default)

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


(use-package! gptel
  :defer t
  :config
  (setq gptel--system-message-alist
        `((default . "")
          (programming . "You are a large language model and a careful programmer. Respond only with code unless explicitly asked.")
          (writing . "You are a large language model and a writing assistant.")
          (chat . "You are a large language model and a conversation partner.")))
  (setq gptel-api-key #'+decrypt-open-ai-token)
  (map! :map gptel-mode-map
        :niv "s-<return>" #'gptel-send))

(use-package! chatgpt-shell
  :defer t
  :commands (chatgpt-shell chatgpt-shell-post-prompt)
  :config
  (setq chatgpt-shell-openai-key
     (auth-source-pick-first-password :host "api.openai.com")))
