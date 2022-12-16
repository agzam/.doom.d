;;; custom/notmuch/autoload/my-notmuch-autoload.el -*- lexical-binding: t; -*-

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
        ;; find the message selected in notmuch-tree buffer, in the thread inside
        ;; notmuch-show buffer
        ;; (notmuch-thread-navigation-mode +1)
        (dolist (_ (notmuch-show-get-messages-ids))
          (let ((msg-id (notmuch-show-get-message-id :bare)))
            (if (string-equal msg-id message-id)
                (let ((props (notmuch-show-get-message-properties)))
                  (notmuch-show-message-visible
                   props
                   (not (plist-get props :message-visible))))
              (notmuch-show-goto-message-next))))
        (call-interactively #'notmuch-show-open-or-close-all)))))

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

;;;###autoload
(defun +notmuch-search-mark-read ()
  (interactive)
  (when (member "unread" (notmuch-search-get-tags))
    (notmuch-search-remove-tag '("-unread")))
  (evil-next-visual-line))

;;;###autoload
(defvar notmuch-thread-navigation-map
  (make-sparse-keymap))

;;;###autoload
(define-minor-mode notmuch-thread-navigation-mode
  "Minor mode for easier jumping through email threads."
  :init-value nil
  :lighter " notmuch thread"
  :keymap notmuch-thread-navigation-map
  :group 'notmuch
  (evil-normal-state))

;;;###autoload
(defun notmuch-thread-navigation-prev ()
  (interactive)
  (cl-case major-mode
    (notmuch-tree-mode
     (notmuch-tree-prev-thread-in-tree))

    (notmuch-show-mode
     (notmuch-show-previous-message))))

;;;###autoload
(defun notmuch-thread-navigation-next ()
  (interactive)
  (cl-case major-mode
    (notmuch-tree-mode
     (notmuch-tree-next-thread-in-tree))

    (notmuch-show-mode
     (notmuch-show-next-message))))

;;;###autoload
(defun +notmuch-get-message-id ()
  "Retrieves message-id."
  (when-let ((id-fn (cl-case major-mode
                      (notmuch-tree-mode #'notmuch-tree-get-message-id)
                      (notmuch-show-mode #'notmuch-show-get-message-id))))
    (funcall id-fn :bare)))

;;;###autoload
(defun +notmuch-open-in-gmail ()
  (interactive)
  (when-let* ((msg-id (+notmuch-get-message-id))
              (url (concat
                    "https://mail.google.com/mail/u/0/#search/rfc822msgid"
                    (url-hexify-string (concat ":" msg-id)))))
    (message "opening url:%s" url)
    (browse-url url)))

;;;###autoload
(defun +notmuch-find-in-mailing-list ()
  "Find message in mailing-list archives"
  (interactive)
  (let* ((mailing-groups '("gnu.org" "googlegroups.com"))
         (headers (plist-get (notmuch-show-get-message-properties) :headers))
         (send-to (concat (plist-get headers :To) ", " (plist-get headers :Cc)))
         (msg-id (notmuch-show-get-message-id :bare))

         ;; figure out the mailing-group index by finding first matching
         ;; address in send-to field
         (mlist (cl-some
                 (lambda (x)
                   (when (string-match (concat "\\([[:graph:]]*\\)@" x) send-to)
                     ;; email addresses often contain < and >, e.g.: Vasya Pupkin <vasya@mail.ru>
                     `(,(replace-regexp-in-string "<\\|>" "" (match-string 1 send-to)) ,x)))
                 mailing-groups))

         (url
          (pcase mlist
            ;; gnu.org
            ;; for some reason it's now broken. It looks like
            ;; something has changed in the portal
            ;; ((pred (lambda (x) (string-match-p "gnu.org" (cadr x))))
            ;;  (concat
            ;;   "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
            ;;   (concat
            ;;    (url-hexify-string
            ;;     (concat
            ;;      "+message-id:<"
            ;;      msg-id
            ;;      ">"))
            ;;    "&submit=" (url-hexify-string "Search!")
            ;;    "&idxname="
            ;;    (car mlist))))

            ((pred (lambda (_) (string-match-p "emacs-orgmode@gnu.org" send-to)))
             (format
              "https://list.orgmode.org/orgmode/%s/"
              (url-hexify-string msg-id)))

            ((pred (lambda (x) (string-match-p "gnu.org" (cadr x))))
             (format
              "https://yhetil.org/%s/%s"
              (car mlist)
              (url-hexify-string msg-id)))

            ;; google.groups
            ((pred (lambda (x) (string-match-p "googlegroups.com" (cadr x))))
             (concat
              "https://groups.google.com/forum/#!topicsearchin/"
              (car mlist)
              "/messageid$3A"
              (url-hexify-string (concat "\"" msg-id "\"")))))))
    (when url
      (message "opening url: " url)
      (browse-url url))))
