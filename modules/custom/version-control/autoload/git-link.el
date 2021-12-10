;;; custom/version-control/autoload/git-link.el -*- lexical-binding: t; -*-

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
  (let ((link (pcase major-mode
                ((pred (lambda (x) (string-match-p "forge-topic" (symbol-name x))))
                 (git-link-forge-topic))

                ((pred (lambda (x) (string-match-p "magit" (symbol-name x))))
                 (message (browse-at-remote-kill)))

                (_ (let* ((git-link-open-in-browser nil)
                          (l1 (line-number-at-pos
                               (when (region-active-p)
                                 (region-beginning))))
                          (l2 (when (region-active-p)
                                (line-number-at-pos
                                 (- (region-end) 1)))))
                     (git-link (git-link--remote) l1 l2))))))
    (when browse? (browse-url link))))

;;;###autoload
(defun git-link-forge-topic ()
  (interactive)
  (let ((url (forge-get-url (forge-current-topic))))
    (message url)
    (kill-new url)))
