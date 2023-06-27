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
  (require 'ob-chatgpt-shell)
  (ob-chatgpt-shell-setup)
  (setq chatgpt-shell-openai-key
        (auth-source-pick-first-password :host "api.openai.com")
        chatgpt-shell-request-timeout 180)

  (add-to-list
   'chatgpt-shell-system-prompts
   `("Cybersecurity" .
     ,(concat "The user is an aspiring cybersecurity expert. "
              "You need to go as deep into technical details as possible. "
              "Elaborate your answers for the highest level of expertise. "
              "Do not expand abbreviations unless explicitly asked. "
              "Code examples should be in Emacs Org-mode source blocks. "
              "Cite relevant RFCs and CVEs, if any. "
              "Links and citations should be in Org-mode link format.")))

  (after! chatgpt-shell
   (map! :map chatgpt-shell-mode-map
         "C-c C-l" #'chatgpt-shell-clear-buffer
         (:localleader
          "s" #'chatgpt-shell-swap-system-prompt)
         :map comint-mode-map
         "C-c C-l" #'comint-clear-buffer))

  (add-hook! 'comint-mode-hook #'cape-completion-at-point-functions-h))

(use-package! dall-e-shell
  :defer t
  :commands (dall-e-shell)
  :config
  (require 'ob-dall-e-shell)
  (ob-dall-e-shell-setup)
  (setq dall-e-shell-openai-key
        (auth-source-pick-first-password :host "api.openai.com")))
