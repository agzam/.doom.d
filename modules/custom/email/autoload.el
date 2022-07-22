;;; custom/email/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +notmuch-message-id->thread-id (message-id)
  "Finds thread-id for given MESSAGE-ID"
  (when-let ((th (ignore-errors
                   (notmuch-call-notmuch-sexp "search" (format "mid:%s" message-id)))))
    (replace-regexp-in-string "thread:" "" (symbol-name th))))

;;;###autoload
(defun +notmuch-tree-show-thread (&optional elide-toggle)
  "Similar to `notmuch-search-show-thread' but for the tree-view.
   Shows the message thread and focuses on the selected message in the thread."
  (interactive "P")
  (when-let* ((props (notmuch-tree-get-message-properties))
              (message-id (plist-get props ':id))
              (thread-id (or (+notmuch-message-id->thread-id message-id)
                             (format "id:%s" message-id)))
              (subject (plist-get (plist-get props ':headers) ':Subject)))
    (notmuch-tree-close-message-window)
    (setq notmuch-tree-message-window (+evil/window-vsplit-and-follow))
    (let ((buf (notmuch-show
                thread-id
                nil
                (current-buffer)
                (notmuch-tree-get-query)
                ;; Name the buffer based on the subject.
                (format "*%s*" (truncate-string-to-width
                                subject
                                30 nil nil t)))))
      (with-current-buffer buf
        (goto-char (point-min))
        (dolist (_ (notmuch-show-get-messages-ids))
          (let ((msg-id (notmuch-show-get-message-id :bare)))
            (if (string-equal msg-id message-id)
                (let ((props (notmuch-show-get-message-properties)))
                  (notmuch-show-message-visible
                   props
                   (not (plist-get props :message-visible))))
              (notmuch-show-goto-message-next))))))))

;;;###autoload
(defun +notmuch-tree-thread-mark-delete ()
  (interactive)
  (let* ((props (notmuch-tree-get-message-properties))
         (trashed? (seq-contains-p (plist-get props ':tags) "trash"))
         (orig-tags (seq-map (lambda (x)
                               (concat "+" x))
                             (plist-get props ':orig-tags))))
    (notmuch-tree-thread-mapcar
     (lambda ()
       (if trashed?
           (progn
             ;; (notmuch-tree-remove-tag '("trash"))
             (notmuch-tree-tag (append orig-tags '("-trash"))))
         (notmuch-tree-tag +notmuch-delete-tags))))))
