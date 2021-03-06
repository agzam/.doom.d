;;; custom/email/config.el -*- lexical-binding: t; -*-

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-queue-mail nil
      smtpmail-queue-dir "~/.mail/queue/cur"
      ;; send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      ;; mail-specify-envelope-from t
      ;; mail-envelope-from 'header

      smtpmail-smtp-user "agzam.ibragimov@gmail.com"
      smtpmail-mail-address "agzam.ibragimov@gmail.com"
      user-mail-address "agzam.ibragimov@gmail.com"
      smtpmail-debug-info t
      smtpmail-debug-verb t
      auth-source-debug t
      auth-source-do-cache nil)

(after! notmuch
  (setq notmuch-search-oldest-first nil
        message-confirm-send t

        ;; needed so lsp-grammarly works
        message-auto-save-directory "/tmp"
        mm-text-html-renderer 'shr)

  (map! (:map notmuch-show-mode-map
         :n "q" #'kill-buffer-and-window)
        (:map notmuch-tree-mode-map
         :n "RET" #'+notmuch-tree-show-thread
         :n "C-<return>" #'notmuch-tree-show-message))

  (map! (:map notmuch-search-mode-map
         :desc "trash" :n "d" #'+notmuch/search-delete
         (:localleader
          :desc "trash" "d" #'+notmuch/search-delete
          :desc "->tree"  "t" #'notmuch-tree-from-search-current-query))
        (:map notmuch-tree-mode-map
         :desc "mark delete" :n "d" #'+notmuch/tree-delete
         :desc "thread mark delete" :n "D" #'+notmuch-tree-thread-mark-delete
         (:localleader
          :desc "thread mark delete" "D" #'+notmuch-tree-thread-mark-delete))
        (:map notmuch-show-mode-map
         :desc "mark delete" :n "d" #'+notmuch/show-delete))

  ;; in tree-view show the message in vertical split (on the right)
  (defadvice! notmuch-tree-vsplit-a ()
    :after 'notmuch-tree-show-message-in
    (with-window-non-dedicated
        notmuch-tree-message-window
      (funcall #'+evil/window-move-left))))


;; (notmuch-search
;;  (plist-get
;;   (cl-first
;;    (seq-filter
;;     (lambda (x) (string-equal (plist-get x :name) "inbox"))
;;     notmuch-saved-searches))
;;   :query))
