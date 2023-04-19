;;; custom/chat/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +decrypt-open-ai-token ()
  "Retrieves encrypted open-ai token from auth-sources."
  (interactive)
  (if (eq gptel-api-key '+decrypt-open-ai-token)
      (setq gptel-api-key
            (funcall
             (plist-get
              (car
               (auth-source-search :host "api.openai.com" :login "gptel" :type 'netrc :max 1))
              :secret)))
    gptel-api-key))

(defun +replace-region-with-string (replacement)
  "Replace region or buffer content with REPLACEMENT."
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point-min) (point-max)))
  (insert replacement))

(defvar chatgpt-improve-text-hist
  '("Improve this text, don't be too formal:"
    "Improve this code:"
    "Add comments to the following code snippet:"
    "Improve and make it witty:"
    "Improve and add some humor:"))

;;;###autoload
(defun +chatgpt-shell-improve-text (prompt-str)
  "Send given text to chat-gpt for given PROMPT-STR."
  (interactive "P")
  (message "beep-bop... checking your crap...")
  (let* ((text (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning)
                    (region-end))
                 (buffer-substring-no-properties
                  (point-min)
                  (point-max))))
         (default-prompt "Improve the following text:")
         (prompt (if prompt-str
                     (read-string "Prompt to use: "
                                  default-prompt
                                  'chatgpt-improve-text-hist)
                   default-prompt))
         (new-text (chatgpt-shell-post-prompt
                    (format "%s\n%s" prompt text)))
         (fst-buf (with-current-buffer (generate-new-buffer " * chat-gpt text 1 *")
                    (insert text)
                    (current-buffer)))
         (snd-buf (with-current-buffer (generate-new-buffer " * chat-gpt text 2 *")
                    (insert new-text)
                    (current-buffer)))
         (diff-win (diff fst-buf snd-buf "--text" 'no-async)))
    (+replace-region-with-string new-text)
    (message "I hope you like it")

    ;; cleaner diff
    (with-current-buffer (window-buffer diff-win)
      (read-only-mode -1)
      (goto-char (point-min))
      (dolist (r '("^diff.*\n"
                   "^. No newline at end of file\n"
                   "^. No newline at end of file\n"
                   "^Diff finished.*$"))
        (re-search-forward r nil :noerror)
        (replace-match ""))
      (visual-line-mode))
    (kill-buffer fst-buf)
    (kill-buffer snd-buf)))
