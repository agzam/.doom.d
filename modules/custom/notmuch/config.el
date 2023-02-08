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

(defvar +notmuch-home-function #'notmuch
  "Function for customizing the landing page for doom-emacs =notmuch.")

(defvar +notmuch-sync-backend 'gmi)

(defvar +notmuch-mail-folder "~/.mail/account.gmail"
  "Where your email folder is located (for use with gmailieer).")

(defvar +notmuch-delete-tags '("+trash" "-inbox" "-unread")
  "Tags applied to mark email for deletion.

When replacing the +trash tag by a different tag such as
+deleted, you will need to update the notmuch-saved-searches
variable accordingly.")

(defvar +notmuch-spam-tags '("+spam" "-inbox" "-unread")
  "Tags applied to mark email as spam.")

(use-package! notmuch
  :commands (notmuch)
  :config
  (defadvice! +notmuch-search-show-thread-a (fn &rest args)
    "Give email buffers a sane name so they can be targeted via
`display-buffer-alist' (and the :ui popup module)."
    :around #'notmuch-search-show-thread
    (letf! (defun notmuch-show (thread-id &optional elide-toggle parent-buffer query-context buffer-name)
             (funcall notmuch-show
                      thread-id elide-toggle parent-buffer query-context
                      (format "*subject:%s*" (substring buffer-name 1 -1))))
      (apply fn args)))

  (setq notmuch-fcc-dirs nil
        message-kill-buffer-on-exit t
        message-fill-column nil        ; do not auto-fill
        send-mail-function 'sendmail-send-it
        ;; sendmail-program "/usr/local/bin/msmtp"
        notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-30s ")
          ("subject" . "%-72s ")
          ("tags" . "(%s)"))
        notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread)))
        notmuch-saved-searches
        '((:name "inbox"   :query "tag:inbox not tag:trash" :key "i")
          (:name "flagged" :query "tag:flagged"             :key "f")
          (:name "sent"    :query "tag:sent"                :key "s")
          (:name "drafts"  :query "tag:draft"               :key "d"))
        notmuch-archive-tags '("-inbox" "-unread"))

  (setq-default notmuch-search-oldest-first nil)
  (setq message-confirm-send t
        ;; needed so lsp-grammarly works
        message-auto-save-directory "/tmp"
        mm-text-html-renderer 'shr)

  ;; only unfold unread messages in thread by default
  (add-hook 'notmuch-show-hook #'+notmuch-show-expand-only-unread-h)
  (add-hook 'doom-real-buffer-functions #'notmuch-interesting-buffer)
  (add-hook! 'notmuch-tree-mode-hook #'notmuch-thread-navigation-mode)
  (advice-add #'notmuch-start-notmuch-sentinel :around #'+notmuch-dont-confirm-on-kill-process-a)

  (map! (:map notmuch-show-mode-map
         :n "q" #'kill-buffer-and-window
         (:localleader
          (:prefix ("o" . "open")
           :desc "open in Gmail" "g" #'+notmuch-open-in-gmail
           :desc "find in mailing list" "m" #'+notmuch-find-in-mailing-list)))
        (:map notmuch-tree-mode-map
         :n "RET" #'+notmuch-tree-show-thread
         :n "C-<return>" #'notmuch-tree-show-message))

  (map! (:map notmuch-search-mode-map
         :desc "trash" :n "d" #'+notmuch/search-delete
         :desc "mark read" :n "u" #'+notmuch-search-mark-read
         (:localleader
          :desc "trash" "d" #'+notmuch/search-delete
          :desc "->tree"  "t" #'notmuch-tree-from-search-current-query))
        (:map notmuch-tree-mode-map
         :desc "mark delete" :n "d" #'+notmuch/tree-delete
         :desc "thread mark delete" :n "D" #'+notmuch-tree-thread-mark-delete
         (:localleader
          :desc "thread mark delete" "D" #'+notmuch-tree-thread-mark-delete
          :desc "thread nav" "T" #'notmuch-thread-navigation-mode
          (:prefix ("o" . "open")
           :desc "open in Gmail" "g" #'+notmuch-open-in-gmail
           :desc "find in mailing list" "m" #'+notmuch-find-in-mailing-list)))
        (:map notmuch-show-mode-map
         :desc "mark delete" :n "d" #'+notmuch/show-delete
         (:localleader
          :desc "thread nav" "T" #'notmuch-thread-navigation-mode
          (:prefix ("o" . "open")
           :desc "open in Gmail" "g" #'+notmuch-open-in-gmail
           :desc "find in mailing list" "m" #'+notmuch-find-in-mailing-list)))
        (:map notmuch-hello-mode-map
              [remap imenu] #'notmuch-search-by-tag))

  (map! :map notmuch-thread-navigation-map
        :n "k" #'notmuch-thread-navigation-prev
        :n "j" #'notmuch-thread-navigation-next
        :n "C-k" #'evil-previous-visual-line
        :n "C-j" #'evil-next-visual-line
        :n "d" (cmd! () (+notmuch-tree-thread-mark-delete)
                     (notmuch-thread-navigation-next))
        :n "C-d" #'+notmuch/tree-delete)

  (map! :localleader
        :map (notmuch-hello-mode-map notmuch-search-mode-map notmuch-tree-mode-map notmuch-show-mode-map)
        :desc "Compose email"   "c" #'+notmuch/compose
        :desc "Sync email"      "u" #'+notmuch/update
        :desc "Quit notmuch"    "q" #'+notmuch/quit
        :map notmuch-search-mode-map
        :desc "Mark as deleted" "d" #'+notmuch/search-delete
        :desc "Mark as spam"    "s" #'+notmuch/search-spam
        :map notmuch-tree-mode-map
        :desc "Mark as deleted" "d" #'+notmuch/tree-delete
        :desc "Mark as spam"    "s" #'+notmuch/tree-spam)

  ;; in tree-view show the message in vertical split (on the right)
  (defadvice! notmuch-tree-vsplit-a ()
    :after 'notmuch-tree-show-message-in
    (with-window-non-dedicated
        notmuch-tree-message-window
      (funcall #'+evil/window-move-left))))

(use-package! org-mime
  :when (modulep! +org)
  :after (org notmuch)
  :config (setq org-mime-library 'mml))

(use-package! consult-notmuch
  :when (modulep! :completion vertico)
  :commands consult-notmuch
  :after notmuch)

;; (notmuch-search
;;  (plist-get
;;   (cl-first
;;    (seq-filter
;;     (lambda (x) (string-equal (plist-get x :name) "inbox"))
;;     notmuch-saved-searches))
;;   :query))
