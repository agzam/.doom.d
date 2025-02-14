;;; custom/ai/config.el -*- lexical-binding: t; -*-

(use-package! whisper
  :config
  (setq whisper-install-directory "/home/ag/sandbox/"
        whisper-model "base"
        whisper-language "en"))

(use-package! gptel
  :commands ()
  :config
  (setq
   gptel-default-mode 'org-mode
   gptel-api-key (auth-host->pass "api.openai.com")
   gptel-expert-commands t)

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "* ")

  (setf
   (cdr (assoc 'default gptel-directives))
   "You are a large language model and an experienced software engineer. Respond concisely. Prioritize theory, don't provide code snippets until instructed. Do not repeat entire body of code (unless explicitly asked for it), only specific part(s). Do not explain the code unless explicitly asked for it.")

  (gptel-make-anthropic "Claude"
    :stream t
    :key (auth-host->pass "antropic.com"))

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream nil
    :models '("llama3:latest" "solar"))

  (add-hook! 'gptel-mode-hook
    (defun gptel-mode-set-local-keys ()
      (map! :map gptel-mode-map
            :i "s-<return>" #'gptel-send
            :i "s-RET" #'gptel-send
            :i ", m" #'gptel-menu
            :i ", SPC" #'insert-comma
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
     (window . root))))

(use-package! gptel-quick
  :commands (gptel-quick)
  :config
  (map! :n "C-s-k" #'gptel-quick)
  (map! :map visual-line-mode-map
        "C-s-k" #'gptel-quick))
