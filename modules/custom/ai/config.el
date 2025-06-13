;;; custom/ai/config.el -*- lexical-binding: t; -*-

(use-package! whisper
  :defer t
  :config
  (setq whisper-install-directory "/home/ag/sandbox/"
        whisper-model "base"
        whisper-language "en"))

(use-package! gptel
  :after (transient)
  :commands (gptel-menu gptel-send)
  :config
  (setf
   (cdr (assoc 'default gptel-directives))
   "You are an experienced software engineer assistant. Respond concisely. Prioritize theory. Do not provide code snippets until instructed. Do not repeat entire snippets of code - show only relevant changes. Do not explain code.")

  (setf
   (cdr (assoc 'chat gptel-directives))
   "You are conversation partner helping me learn and improve Spanish. Respond concisely. Point to the mistakes I make. Suggest improvements. Help me to acquire the language. Share interesting etymological facts, e.g., explaining why certain words are feminine due to their Greek origin.")

  (setopt
   gptel-default-mode 'org-mode
   gptel-expert-commands t
   gptel-track-media t)

  (setq gptel-api-key (lambda () (auth-host->pass "api.openai.com")))

  (after! gptel-transient
    (transient-suffix-put 'gptel-menu (kbd "RET") :key "s-<return>"))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "* ")

  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda () (auth-host->pass "antropic.com")))

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream nil
    :models '("llama3:latest" "solar"))

  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (lambda () (auth-host->pass "deepseek.com"))
    :models '(deepseek-chat deepseek-reasoner deepseek-coder))

  (add-hook! 'gptel-mode-hook
    (defun gptel-mode-set-local-keys ()
      (map! :map gptel-mode-map
            :i "s-<return>" #'gptel-send
            :i "s-RET" #'gptel-send
            :i ", m" #'gptel-menu
            :i ", SPC" #'insert-comma
            :n "q" #'bury-buffer
            (:localleader
             "m" #'gptel-menu
             "," #'gptel-menu
             (:prefix ("s" . "session")
              :desc "clear" "l" #'gptel-clear-buffer+)))))

  (add-hook! 'kill-emacs-hook
    (defun persist-gptel-model ()
      (customize-save-variable 'gptel-backend gptel-backend)
      (customize-save-variable 'gptel-model gptel-model)))

  (add-hook! 'gptel-post-stream-hook #'gptel-persist-history)

  (add-to-list
   'display-buffer-alist
   `(,(rx bos (or "*Claude" "*ChatGPT" "gptel-"))
     (display-buffer-in-quadrant)
     (direction . right)
     (window . root)))

  (add-to-list
   'display-buffer-alist
   `((lambda (buffer _action)
       (with-current-buffer buffer
         (and buffer-file-name
              (natnump (string-match-p
                        (concat org-default-folder "gptel/quick.org")
                        buffer-file-name)))))

     (display-buffer-in-quadrant)
     (direction . right)
     (window . root))))

(use-package! gptel-quick
  :commands (gptel-quick)
  :config
  (map! :n "C-s-k" #'gptel-quick)
  (map! :map visual-line-mode-map
        "C-s-k" #'gptel-quick))

(use-package! aider
  :commands (aider-transient-menu)
  :hook (aider-comint-mode . visual-line-mode)
  :config
  (require 'aider-doom)
  (setopt aider-args `("--model" "claude-opus-4-20250514"
                       "--no-auto-commits"
                       "--anthropic-api-key" ,(auth-host->pass "antropic.com"))))

(use-package! khoj
  :after (org org-roam)
  :config
  (setopt
   khoj-index-directories (list
                           org-default-folder
                           (expand-file-name "~/SyncMobile/Books")
                           (expand-file-name "~/SyncMobile/Papers"))
   khoj-server-url "http://127.0.0.1:42110"))
