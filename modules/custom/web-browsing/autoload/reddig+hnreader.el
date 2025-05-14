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
  (unless (string-match-p
           "\\*HN\\*\\|\\*reddigg\\*"
           (buffer-name))
    (org-fold-show-all))
  (setq-local org-link-elisp-confirm-function nil)
  (run-with-timer
   0.3 nil
   (lambda ()
     (goto-char (point-min))
     (jinx-mode -1)
     (ignore-errors
       (org-next-visible-heading 1)))))

;;;###autoload
(defun consult-line-collect-urls (&optional ignore-regexp)
  "Like consult-line for urls, excluding `ignore-regexp'."
  (interactive)
  (let* ((url-regex "http.://[^[:space:]()<>]+")
         (buffer-lines (with-current-buffer (current-buffer)
                         (split-string (buffer-string) "\n")))
         (line-candidates '())
         (line-number 0))

    ;; Build the candidate list with only URLs
    (dolist (line buffer-lines)
      (setq line-number (1+ line-number))
      (when (and (string-match url-regex line)
                 (not (string-match (or ignore-regexp "🐜+") line))) ; just needed some never-matching bogus regexp
        ;; Extract just the URL from the line
        (let ((url (match-string 0 line)))
          (push (cons (format "%d: %s" line-number url) line-number) line-candidates))))

    ;; Use consult to select a candidate
    (let ((selected (consult--read
                     (reverse line-candidates)
                     :prompt "URL: "
                     :category 'consult-line
                     :sort nil
                     :require-match t)))
      (when selected
        ;; Jump to the selected line
        (let ((line-num (cdr (assoc selected line-candidates))))
          (goto-char (point-min))
          (forward-line (1- line-num))
          (recenter))))))
