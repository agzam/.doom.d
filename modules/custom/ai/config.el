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
   "You are an experienced software engineer assistant. Respond concisely. Prioritize theory. Do not provide code snippets until instructed. Do not repeat entire snippets of code - show only relevant changes, unless instructed otherwise. Do not explain code. Do not replace backticks and other symbols in the code to accommodate for Org-mode - keep the code in source blocks as independent pieces that have nothing to do with Org-mode markup.")

  (setf
   (cdr (assoc 'chat gptel-directives))
   "You are conversation partner helping me learn and improve Spanish. Respond concisely. Point to the mistakes I make. Suggest improvements. Help me to acquire the language. Share interesting etymological facts, e.g., explaining why certain words are feminine due to their Greek origin.")

  (setopt
   gptel-default-mode 'org-mode
   gptel-expert-commands t
   gptel-track-media t)

  (setq gptel-api-key (lambda () (auth-host->pass "api.openai.com")))

  (after! gptel-transient
    (transient-suffix-put 'gptel-menu (kbd "RET") :key "s-<return>")
    (transient-suffix-put 'gptel-tools (kbd "RET") :key "s-<return>"))

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

  (map! "C-c C-g" #'gptel-abort)

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
     (
      ;; display-buffer-reuse-window
      display-buffer-in-quadrant)
     (init-width . 0.40)
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
     (display-buffer-reuse-window
      display-buffer-in-quadrant)
     (direction . right)
     (window . root))))

(use-package! gptel-quick
  :commands (gptel-quick)
  :config
  (map! :n "C-s-k" #'gptel-quick)
  (map! :map visual-line-mode-map
        "C-s-k" #'gptel-quick))

(use-package! ob-gptel
  :after (org gptel)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gptel . t))))

(use-package! llm-tool-collection
  :after gptel
  :config
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-all)))

(use-package! gptel-tools
  :after gptel
  :config
  (gptel-make-preset 'websearch
    :description "Add basic web search tools"
    :pre (lambda () (require 'gptel-tools))
    :tools '(:append "search_web" "read_url" "get_youtube_meta")))

(use-package! ragmacs
  :after gptel
  :init
  (gptel-make-preset 'introspect
    :pre (lambda () (require 'ragmacs))
    :description "Introspect Emacs with Ragmacs"
    :system
    "You are pair programming with the user in Emacs and on Emacs.

Your job is to dive into Elisp code and understand the APIs and
structure of elisp libraries and Emacs.  Use the provided tools to do
so, but do not make duplicate tool calls for information already
available in the chat.

<tone>
1. Be terse and to the point.  Speak directly.
2. Explain your reasoning.
3. Do NOT hedge or qualify.
4. If you don't know, say you don't know.
5. Do not offer unprompted advice or clarifications.
6. Never apologize.
7. Do NOT summarize your answers.
</tone>

<code_generation>
When generating code:
1. Create a plan first: list briefly the design steps or ideas involved.
2. Use the provided tools to check that functions or variables you use
in your code exist.
3. Also check their calling convention and function-arity before you use
them.
</code_generation>

<formatting>
1. When referring to code symbols (variables, functions, tags etc)
enclose them in markdown quotes.
  Examples: `read_file`, `getResponse(url, callback)`
  Example: `<details>...</details>`
2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
</formatting>"
    :cache '(tool)
    :tools '("introspection")))
