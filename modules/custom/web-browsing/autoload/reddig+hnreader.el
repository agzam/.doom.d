;;; custom/web-browsing/autoload/reddig.el -*- lexical-binding: t; -*-

;;;###autoload
(defun reddigg-copy-current-sub-url ()
  "Kill reddigg buffer subreddit url."
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (when-let* ((bound (save-excursion
                        (goto-char (point-min))
                        (forward-line 7)
                        (point)))
               (_ (re-search-forward "\\(https://\\(?:redd\\.it\\|reddit\\.com\\).*\\|/r/[^/]+/comments/.*\"\\)"
                                     bound :no-error))
               (lnk (match-string-no-properties 0))
               (url (if (string-match-p "https://.*" lnk)
                        lnk
                      (format "https://old.reddit.com%s" lnk))))
     (kill-new url)
     (message url)
     url)))

;;;###autoload
(defun reddigg-browse-current-sub-url ()
  "Browse reddigg buffer subreddit url."
  (interactive)
  (when-let* ((url (reddigg-copy-current-sub-url)))
    (browse-url url)))

;;;###autoload
(defun hnreader-copy-hn-story-url ()
  (interactive)
  (save-excursion
    (goto-char (point-max))

    (when-let* ((bound (save-excursion
                         (goto-char (point-max))
                         (beginning-of-line)
                         (point)))
                (_ (re-search-backward
                    "\\[\\[elisp:(hnreader-comment \"\\(.*?\\)\")\\]"
                    bound :no-error))
                (url (match-string-no-properties 1)))
      (kill-new url)
      (message url)
      url)))

;;;###autoload
(defun hnreader-browse-nh-story-url ()
  "Browse reddigg buffer subreddit url."
  (interactive)
  (when-let* ((url (hnreader-copy-hn-story-url)))
    (browse-url url)))

;;;###autoload
(defun reddigg-hnreader-show-all-h ()
  "Expands all the comments."
  (org-fold-show-all)
  (goto-char (point-min))
  (org-next-visible-heading 1))
