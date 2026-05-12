;;; custom/ai/autoload/eca.el -*- lexical-binding: t; -*-

;;;###autoload
(defadvice! eca-auto-trust-for-plan-a (session new-agent &optional buffer)
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

;;;###autoload
(defun eca-chat-flag-and-fork ()
  "Add a flag at point and immediately fork the conversation from it.
Uses async RPCs to avoid `eca-api-request-sync' throw which drops
sibling notifications from the same process-filter batch."
  (interactive)
  (eca-assert-session-running (eca-session))
  (let ((nearest-id nil)
        (nearest-pos -1)
        (session (eca-session)))
    (dolist (ov (overlays-in (point-min) (1+ (point))))
      (when-let* ((id (overlay-get ov 'eca-chat--expandable-content-id))
                  (pos (overlay-start ov)))
        ;; Only consider message overlays (UUID content-ids).
        ;; Skip tool calls (toolu_*), widgets (eca-chat-*), flags.
        (when (and (> pos nearest-pos)
                   (not (overlay-get ov 'eca-chat--flag-text))
                   (not (overlay-get ov 'eca-chat--tool-call-status))
                   (string-match-p "\\`[0-9a-f]\\{8\\}-" id))
          (setq nearest-id id nearest-pos pos))))
    (if (not nearest-id)
        (user-error "No message found before point")
      (let ((origin-title (or eca-chat--title "chat"))
            (existing-flags
             (mapcar (lambda (ov) (overlay-get ov 'eca-chat--expandable-content-id))
                     (seq-filter (lambda (ov) (overlay-get ov 'eca-chat--flag-text))
                                 (overlays-in (point-min) (point-max)))))
            (existing-bufs (eca-vals (eca--session-chats session)))
            (proc (eca--session-process session))
            (deadline (+ (float-time) 10))
            (rpc-err nil))
        ;; Phase 1 - add flag (async so the contentReceived notification
        ;; in the same pipe chunk is not dropped by throw)
        (eca-api-request-async session
          :method "chat/addFlag"
          :params (list :chatId eca-chat--id
                        :contentId nearest-id
                        :text (format-time-string "fork @ %H:%M"))
          :success-callback #'ignore
          :error-callback (lambda (err) (setq rpc-err err)))
        (let ((new-flag-id nil))
          (while (and (not new-flag-id) (not rpc-err)
                      (< (float-time) deadline))
            (accept-process-output proc 0.2)
            (dolist (ov (overlays-in (point-min) (point-max)))
              (when-let* ((_ft (overlay-get ov 'eca-chat--flag-text))
                          (id (overlay-get ov 'eca-chat--expandable-content-id)))
                (unless (member id existing-flags)
                  (setq new-flag-id id)))))
          (when rpc-err (user-error "addFlag failed: %s" rpc-err))
          (unless new-flag-id (user-error "Timed out waiting for flag"))
          ;; Phase 2 - fork (async so the chat/opened notification is
          ;; not dropped either)
          (setq rpc-err nil)
          (eca-api-request-async session
            :method "chat/fork"
            :params (list :chatId eca-chat--id
                          :contentId new-flag-id)
            :success-callback #'ignore
            :error-callback (lambda (err) (setq rpc-err err)))
          (let ((new-buf nil))
            (while (and (not new-buf) (not rpc-err)
                        (< (float-time) deadline))
              (accept-process-output proc 0.2)
              (dolist (buf (eca-vals (eca--session-chats session)))
                (unless (memq buf existing-bufs)
                  (setq new-buf buf))))
            (when rpc-err (user-error "fork failed: %s" rpc-err))
            (if new-buf
                (progn
                  (setf (eca--session-last-chat-buffer session) new-buf)
                  (switch-to-buffer new-buf)
                  (let ((fork-title (format "Fork: %s" origin-title)))
                    (setq-local eca-chat--title fork-title)
                    (eca-api-request-async session
                      :method "chat/update"
                      :params (list :chatId eca-chat--id :title fork-title)
                      :success-callback #'ignore
                      :error-callback #'ignore)
                    (eca-chat--force-tab-line-update)))
              (user-error "Timed out waiting for forked chat"))))))))

;;;###autoload
(defun eca-hide-buffer-name-a (orig-fn &rest args)
  "Prepend a space to make the buffer name hidden."
  (concat " " (apply orig-fn args)))

;;;###autoload
(defun eca-chat-buffer-name (&optional title)
  "Build a chat buffer name from TITLE, falling back to \"Empty chat\"."
  (format "eca-chat - %s" (or title "Empty chat")))

;;;###autoload
(defun eca-chat-new-buffer-name-a (_session)
  "Override chat buffer naming to use a readable title."
  (eca-chat-buffer-name))

;;;###autoload
(defun eca-chat-rename-buffer-on-title-change ()
  "Rename the current chat buffer to reflect the new title."
  (when (derived-mode-p 'eca-chat-mode)
    (let* ((title (or eca-chat--custom-title eca-chat--title "Empty chat"))
           (new-name (eca-chat-buffer-name title)))
      (unless (string= (buffer-name) new-name)
        (rename-buffer new-name t)))))

;;;###autoload
(defun eca-chat-rename-after-content-a (session _params)
  "Rename chat buffer after content-received sets the title."
  (when-let* ((chats (eca--session-chats session)))
    (dolist (pair chats)
      (when (buffer-live-p (cdr pair))
        (with-current-buffer (cdr pair)
          (eca-chat-rename-buffer-on-title-change))))))

;;;###autoload
(defun eca-chat-exit-cleanup-a (orig-fn session)
  "Around advice to fix closed-buffer cleanup for renamed chat buffers."
  (funcall orig-fn session)
  ;; Clean up stale closed chat buffers with new naming scheme.
  (let ((latest nil))
    (dolist (b (buffer-list))
      (when (string-match-p "^eca-chat - .*:closed" (buffer-name b))
        (if latest
            (kill-buffer b)
          (setq latest b))))))

;;;###autoload
(define-minor-mode eca-workspaces-mode
  "Minor mode for keybindings in the eca-workspaces buffer."
  :keymap (make-sparse-keymap))

;;;###autoload
(defun eca-toggle-workspaces ()
  "Toggle the eca-workspaces side window."
  (interactive)
  (if-let* ((buf (get-buffer eca-workspaces-buffer-name))
            (win (get-buffer-window buf t)))
      (delete-window win)
    (eca-workspaces)
    (when-let* ((buf (get-buffer eca-workspaces-buffer-name)))
      (with-current-buffer buf
        (eca-workspaces-mode 1)))))
