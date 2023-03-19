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
