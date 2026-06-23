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
(defadvice! eca-hide-buffer-name-a (orig-fn &rest args)
  "Prepend a space to make the buffer name hidden."
  :around 'eca-process--buffer-name
  :around 'eca-process--stderr-buffer-name
  :around 'eca--emacs-errors-buffer-name
  (concat " " (apply orig-fn args)))

;;;###autoload
(defun eca-chat-buffer-name (&optional title)
  "Build a chat buffer name from TITLE, falling back to \"Empty chat\"."
  (format "eca-chat - %s" (or title "Empty chat")))

;;;###autoload
(defadvice! eca-chat-new-buffer-name-a (_session)
  "Override chat buffer naming to use a readable title."
  :override 'eca-chat-new-buffer-name
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
(defadvice! eca-chat-rename-after-content-a (session _params)
  "Rename chat buffer after content-received sets the title."
  :after 'eca-chat-content-received
  :after 'eca-chat-opened
  (when-let* ((chats (eca--session-chats session)))
    (dolist (pair chats)
      (when (buffer-live-p (cdr pair))
        (with-current-buffer (cdr pair)
          (eca-chat-rename-buffer-on-title-change))))))

;;;###autoload
(defadvice! eca-chat-exit-cleanup-a (orig-fn session)
  "Around advice to fix closed-buffer cleanup for renamed chat buffers."
  :around 'eca-chat-exit
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

;;;###autoload
(defun eca-reauth ()
  "Re-trigger Anthropic Max OAuth login on the current session.
Workaround for the OAuth refresh race condition (eca#462).
After token rotation invalidates this session's refresh-token,
the CLI process stays alive, so re-running the max flow gets
a fresh token pair without losing the active chat."
  (interactive)
  (let ((session (eca-session)))
    (unless session
      (user-error "No active ECA session for current buffer"))
    (unless (eca-process-running-p session)
      (user-error "ECA session is not running"))
    (eca-providers--do-login session "anthropic" "max")))


;;;; Session archiving + resume-from-archive

(defvar eca-archive-dir)                ; real defcustom lives in config.el

(defun eca-archive--slugify (string)
  "Return STRING as a filename-safe slug, or \"\" when blank.
Drops text properties, turns whitespace and characters illegal in
filenames into single hyphens, trims stray hyphens/dots, and caps the
length so titles stay readable but bounded."
  (let* ((s (replace-regexp-in-string
             "[[:cntrl:][:space:]/\\:*?\"<>|]+" "-"
             (substring-no-properties (or string ""))))
         (s (replace-regexp-in-string "-+" "-" s))
         (s (string-trim s "[-.]+" "[-.]+")))
    (if (< 72 (length s))
        (string-trim-right (substring s 0 72) "[-.]+")
      s)))

(defun eca-archive--chat-file-name (project title id-short)
  "Return the archive basename for a chat.
PROJECT and ID-SHORT identify the chat and keep the name unique; TITLE,
when it slugifies to something non-empty, is inserted for readability."
  (let ((slug (eca-archive--slugify title)))
    (if (string-empty-p slug)
        (format "%s_%s.md" project id-short)
      (format "%s__%s_%s.md" project slug id-short))))

;;;###autoload
(defun eca-archive-chat (&optional buffer)
  "Write BUFFER's chat transcript to `eca-archive-dir' as Markdown.
Keeps one file per chat - named after the project, chat title, and
short chat id - re-saved each finished turn.  When the title changes,
the previous file for this chat is removed.  Return the file path, or
nil if BUFFER is not an archivable chat."
  (interactive)
  (with-demoted-errors "eca-archive: %S"
    (with-current-buffer (or buffer (current-buffer))
      (when (and (derived-mode-p 'eca-chat-mode)
                 (stringp eca-chat--id)
                 (not (string-prefix-p "subagent-" eca-chat--id)))
        (let* ((session   (thread-last
                            eca--sessions
                            eca-vals
                            (seq-find (lambda (s)
                                        (memq (current-buffer)
                                              (eca-vals (eca--session-chats s)))))))
               (project   (replace-regexp-in-string
                           "\\`[.]+" ""
                           (if session (eca--session-project-name session) "unknown")))
               (workspace (when session (car (eca--session-workspace-folders session))))
               (model     (or eca-chat--selected-model "unknown"))
               (id        eca-chat--id)
               (id-short  (substring id 0 (min 8 (length id))))
               (title     (when (or eca-chat--custom-title eca-chat--title)
                            (substring-no-properties (eca-chat-title))))
               (dir       (expand-file-name eca-archive-dir))
               (file      (expand-file-name
                           (eca-archive--chat-file-name project title id-short) dir))
               (content   (buffer-substring-no-properties (point-min) (point-max))))
          (make-directory dir t)
          (with-temp-file file
            (insert (format "<!-- eca: %S -->\n\n"
                            (list :id id :workspace workspace :model model)))
            (insert content))
          ;; Keep one file per chat: now that FILE exists, drop any stale
          ;; name for this id (e.g. an earlier untitled save).
          (dolist (old (file-expand-wildcards
                        (expand-file-name (format "*_%s.md" id-short) dir)))
            (unless (file-equal-p old file)
              (ignore-errors (delete-file old))))
          (when (called-interactively-p 'interactive)
            (eca-info "Archived chat to %s" file))
          file)))))

(defun eca-archive--read-meta (file)
  "Return the metadata plist on FILE's first line, or nil."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (goto-char (point-min))
    (let ((line (buffer-substring-no-properties (point) (line-end-position))))
      (when (and (string-prefix-p "<!-- eca: " line)
                 (string-suffix-p " -->" line))
        (read (string-remove-suffix
               " -->" (string-remove-prefix "<!-- eca: " line)))))))

(defun eca-archive--session-for-root (root)
  "Return a running session whose workspace folders include ROOT."
  (when (and (stringp root) (not (string-empty-p root)))
    (let ((root (file-name-as-directory (expand-file-name root))))
      (thread-last
       eca--sessions
       eca-vals
       (seq-find
        (lambda (s)
          (thread-last
           (eca--session-workspace-folders s)
           (seq-some (lambda (f)
                       (string= (file-name-as-directory (expand-file-name f))
                                root))))))))))

(defun eca-archive--open-chat (session chat-id)
  "Open CHAT-ID in SESSION via chat/open and surface its buffer.
Mirrors the open path of `eca-chat-resume'."
  (eca-assert-session-running session)
  (let ((from-buf (current-buffer)))
    (eca-api-request-async
     session
     :method "chat/open"
     :params (append (list :chatId chat-id)
                     (when eca-chat-history-page-size
                       (list :limit eca-chat-history-page-size)))
     :success-callback
     (lambda (open-res)
       (cond
        ((not (plist-get open-res :found?))
         (user-error "Server could not open chat %s" chat-id))
        ((not (buffer-live-p (eca-get (eca--session-chats session) chat-id)))
         (user-error "No buffer registered for chat %s" chat-id))
        (t
         (let ((chat-buf (eca-get (eca--session-chats session) chat-id)))
           (setf (eca--session-last-chat-buffer session) chat-buf)
           (eca-chat--with-current-buffer chat-buf
             (eca-chat--apply-history-meta (plist-get open-res :meta))
             (eca-chat--refresh-load-older-control)
             (eca-chat--protect-non-prompt))
           (eca-chat-open session)
           (eca-chat--kill-empty-welcome-buffer session from-buf chat-buf)))))
     :error-callback
     (lambda (err) (user-error "Failed to continue chat: %s" err)))))

;;;###autoload
(defun eca-continue-from-file (&optional file)
  "Open and continue the ECA chat recorded in archive FILE.
Without FILE, use the current archive buffer or prompt in
`eca-archive-dir'.  Like `eca-chat-resume', the chat's workspace
session must already be running and still hold the chat."
  (interactive)
  (let* ((file (or file
                   (and buffer-file-name
                        (file-in-directory-p buffer-file-name
                                             (expand-file-name eca-archive-dir))
                        buffer-file-name)
                   (read-file-name "Continue ECA chat from: "
                                   (file-name-as-directory
                                    (expand-file-name eca-archive-dir))
                                   nil t)))
         (meta      (eca-archive--read-meta file))
         (chat-id   (plist-get meta :id))
         (workspace (plist-get meta :workspace))
         (session   (eca-archive--session-for-root workspace)))
    (cond
     ((null chat-id)
      (user-error "No chat metadata found in %s" file))
     ((null session)
      (user-error "No running ECA session for %s; start it with `eca' first"
                  (or workspace "that workspace")))
     ((not (eq (eca--session-status session) 'started))
      (user-error "ECA session for %s is not ready yet" workspace))
     (t
      (eca-archive--open-chat session chat-id)))))
