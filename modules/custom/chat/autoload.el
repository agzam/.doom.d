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

;;;###autoload
(defun +chatgpt-shell-check-english-in-place ()
  "Check English and replace text in place."
  (interactive)
  (message "beep-bop... checking your crap...")
  (+replace-region-with-string
   (chatgpt-shell-post-prompt
    (format "Please help me proofread the following text with English:\n%s"
            (if (region-active-p)
                (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))
              (buffer-substring-no-properties
               (point-min)
               (point-max)))))))
