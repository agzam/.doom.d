;;; custom/chat/config.el -*- lexical-binding: t; -*-

(use-package! telega
  :defer t
  :config
  (setq telega-server-libs-prefix "/opt/homebrew/opt/tdlib"))

(use-package! gptel
  :config
  (setq gptel--system-message-alist
        `((default . "")
          (programming . "You are a large language model and a careful programmer. Respond only with code unless explicitly asked.")
          (writing . "You are a large language model and a writing assistant.")
          (chat . "You are a large language model and a conversation partner.")))
  (setq gptel-api-key #'+decrypt-open-ai-token)
  (map! :map gptel-mode-map
        :niv "s-<return>" #'gptel-send))
