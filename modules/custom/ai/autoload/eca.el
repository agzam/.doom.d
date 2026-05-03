;;; custom/ai/autoload/eca.el -*- lexical-binding: t; -*-

;;;###autoload
(defadvice! +eca-auto-trust-for-plan-a (session new-agent &optional buffer)
  "Toggle trust mode based on agent: enable for plan, disable for code."
  :after #'eca-chat--set-agent
  (when-let* ((buf (or buffer (eca-chat--get-last-buffer session))))
    (let ((trust (equal new-agent "plan")))
      (eca-chat--set-trust session trust buf)
      (when-let* ((chat-id (buffer-local-value 'eca-chat--id buf)))
        (eca-api-request-sync session
                              :method "chat/update"
                              :params (list :chatId chat-id
                                            :trust (if trust t :json-false))))
      (eca-info (if trust "Plan mode: trust enabled" "Code mode: trust disabled")))))
