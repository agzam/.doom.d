;;; custom/chat/config.el -*- lexical-binding: t; -*-

(use-package! telega
  :defer t
  :hook (telega-chat . jinx-mode)
  :config
  (setq telega-server-libs-prefix (if IS-MAC "/opt/homebrew/opt/tdlib" "/usr")
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


(use-package! chatgpt-shell
  :defer t
  :commands (chatgpt-shell chatgpt-shell-post-prompt)
  :config
  (require 'ob-chatgpt-shell)
  (ob-chatgpt-shell-setup)
  (setq chatgpt-shell-model-versions '("gpt-3.5-turbo"
                                       "gpt-4"
                                       "gpt-4-32k"
                                       "gpt-4-32k-0613"))

  (setq chatgpt-shell-openai-key
        (auth-source-pick-first-password :host "api.openai.com")
        chatgpt-shell-request-timeout 180
        chatgpt-shell-welcome-function nil)

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

  (add-to-list
   'chatgpt-shell-system-prompts
   `("Espanól" .
     ,(concat "The user is a person trying to learn Spanish. "
              "Expect request texts mixed in both languages."
              "Answer in Spanish, unless specifically asked to provide the translation."
              "Focus on Latin American (primarily Mexican) dialect. "
              "When applicable, help the user with memorization and building vocabulary. "
              "Highligh the connection of words with shared etymology, e.g.: 'quieres' to 'inquire', and 'ayuda' to 'aid'. "
              "When asked about specific words, provide example sentences.")))

  (add-to-list
   'chatgpt-shell-system-prompts
   `("Leetcode" .
     ,(concat "Help user to solve Leetcode problems. "
              "Structure responses in Org-Mode format and org-babel source blocks. "
              "Write solutions in javascript. "
              "Avoid using 'for loops' whenever possible, using .map/.reduce instead. "
              "Comment on time and space complexity of each solution. "
              "Advertise alternative algorithms and approaches for further research. ")))

  ;; set default prompt to None
  (setq chatgpt-shell-system-prompt
        (- (length chatgpt-shell-system-prompts)
           (length (member (assoc "None" chatgpt-shell-system-prompts)
                           chatgpt-shell-system-prompts))))

  (add-hook! chatgpt-shell-mode
    (defun set-chat-gpt-shell-keys-h ()
      (map! :map chatgpt-shell-mode-map
            :i "RET" #'+default/newline
            :i "s-<return>" #'shell-maker-submit
            :i "s-RET" #'shell-maker-submit
            :i
            "C-c C-l" #'chatgpt-shell-clear-buffer
            (:localleader
             "p" #'chatgpt-shell-swap-system-prompt)
            :map comint-mode-map
            "C-c C-l" #'comint-clear-buffer))
    #'jinx-mode)

  (add-hook! comint-mode #'cape-completion-at-point-functions-h))

(use-package! dall-e-shell
  :defer t
  :commands (dall-e-shell)
  :config
  (require 'ob-dall-e-shell)
  (ob-dall-e-shell-setup)
  (setq dall-e-shell-openai-key
        (auth-source-pick-first-password :host "api.openai.com")))

(use-package! openai
  :defer t
  :config
  (setq openai-key #'openai-key-auth-source)




  )
