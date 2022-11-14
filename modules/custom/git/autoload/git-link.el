;;; custom/git/autoload/git-link.el -*- lexical-binding: t; -*-

;;;###autoload
(defun git-link-main-branch (&optional browse?)
  (interactive "P")
  (require 'git-link)
  (let* ((git-link-default-branch (magit-main-branch)))
    (call-interactively #'git-link-kill)))

;;;###autoload
(defun git-link-blame ()
  (interactive)
  (cl-flet ((git-link--new* (x) (replace-regexp-in-string "/blob/" "/blame/" x)))
    (advice-add 'git-link--new :override #'git-link--new*)
    (let ((link (call-interactively 'git-link)))
      (advice-remove 'git-link--new #'git-link--new*)
      (git-link--new link))))

;;;###autoload
(defun git-link-kill (&optional browse?)
  "Copy URL to current file/revision/forge-topic"
  (interactive "P")
  (require 'git-link)
  (cl-letf (((symbol-function #'git-link--new) (lambda (link) link)))
    (let ((link (pcase major-mode
                  ((pred (lambda (x) (string-match-p "forge-topic" (symbol-name x))))
                   (git-link-forge-topic))

                  ((pred (lambda (x) (string-match-p "magit" (symbol-name x))))
                   (message (browse-at-remote-kill)))

                  ;; #'git-link fn detestably has been made to be exclusively called
                  ;; interactively, so I had to temporarily redefine #'git-link--new
                  ;; (above), ignore prefix arg and other parameters, in order to retrieve
                  ;; the link
                  (_ (let* ((current-prefix-arg nil)
                           (git-link-open-in-browser browse?)
                           (lnk (call-interactively #'git-link)))
                       (kill-new lnk)
                       (message lnk))))))
      (when browse? (browse-url link)))))

;;;###autoload
(defun git-link-forge-topic ()
  (interactive)
  (let ((url (forge-get-url (forge-current-topic))))
    (message url)
    (kill-new url)))
