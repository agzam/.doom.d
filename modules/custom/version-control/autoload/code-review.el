;;; custom/version-control/autoload/code-review.el -*- lexical-binding: t; -*-

;;;###autoload
(defun code-review-browse-pr+ ()
  (interactive)
  (browse-url
   (let-alist (code-review-db-get-pr-alist)
     (format "https://github.com/%s/%s/pull/%s" .owner .repo .num))))
