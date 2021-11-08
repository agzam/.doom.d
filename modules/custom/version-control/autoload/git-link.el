;;; custom/version-control/autoload/git-link.el -*- lexical-binding: t; -*-

;;;###autoload
(defun git-link-master-branch ()
    (interactive)
    (require 'git-link)
    (let ((git-link-default-branch "master"))
      (call-interactively #'git-link)))
