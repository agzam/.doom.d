;;; custom/writing/autoload/markdown.el -*- lexical-binding: t; -*-

;;;###autoload
(defun yank-as-org ()
  "Convert region of markdown text to org while yanking."
  (interactive)
  (let* ((_ (unless (executable-find "pandoc")
              (user-error "pandoc not found")))
         (beg (if evil-mode
                  (marker-position evil-visual-beginning)
                (region-beginning)))
         (end (if evil-mode
                  (marker-position evil-visual-end)
                (region-end)))
         (region-content (buffer-substring-no-properties beg end))
         (_ (print region-content))
         (converted-content
          (with-temp-buffer
            (insert region-content)
            (shell-command-on-region
             (point-min)
             (point-max)
             "pandoc --wrap=none -f markdown -t org" nil t)
            (buffer-string))))
    (kill-new converted-content)
    (message "yanked Markdown as Org")))

;;;###autoload
(defun yank-as-markdown ()
  "Convert region of Org-mode to markdown while yanking."
  (interactive)
  (let* ((_ (unless (executable-find "pandoc")
              (user-error "pandoc not found")))
         (beg (if evil-mode
                  (marker-position evil-visual-beginning)
                (region-beginning)))
         (end (if evil-mode
                  (marker-position evil-visual-end)
                (region-end)))
         (region-content (buffer-substring-no-properties beg end))
         (_ (print region-content))
         (converted-content
          (with-temp-buffer
            (insert region-content)
            (shell-command-on-region
             (point-min)
             (point-max)
             "pandoc --wrap=none -f org -t gfm | sed 's/<sub>\\([^<]*\\)<\\/sub>/_\\1/g'" nil t)
            (buffer-string))))
    (kill-new converted-content)
    (message "yanked Org as Markdown")))

;;;###autoload
(defun maybe-yank-and-convert-a (orig-fun beg end &optional type register yank-handler)
  "Advice function to convert marked region to org before yanking."
  (let ((yank-fn (cond
                  ((derived-mode-p 'markdown-mode)
                   #'yank-as-org)
                  ((derived-mode-p 'org-mode)
                   #'yank-as-markdown))))
    (if (and current-prefix-arg (use-region-p)
             yank-fn)
        (funcall yank-fn)
      (funcall
       orig-fun
       beg end type register
       yank-handler))))

;;;###autoload
(defun markdown-wrap-collapsible ()
  "Wrap region in a collapsible section."
  (interactive)
  (when (region-active-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (content (delete-and-extract-region beg end))
           ;; typically content inside collapsible needs indentation
           (content (with-temp-buffer
                      (insert content)
                      (indent-rigidly (point-min) (point-max) 4)
                      (buffer-substring (point-min) (point-max)))))
      (insert
       (format "<details>\n  <summary></summary>\n\n%s\n\n</details>" content))
      (goto-char beg)
      (search-forward "<summary>")
      (when evil-mode
        (evil-insert-state)))))

;;;###autoload
(defun markdown-wrap-code-generic ()
  "Wrap region in a collapsible section."
  (interactive)
  (when (region-active-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (content (buffer-substring beg end)))
      (delete-region beg end)
      (deactivate-mark)
      (insert
       (format "```\n%s\n```\n" content))
      (search-backward "```" nil :noerror 2)
      (forward-char 3)
      (when evil-mode
        (evil-insert-state)))))

;;;###autoload
(defun markdown-wrap-code-clojure ()
  "Wrap region in a collapsible section."
  (interactive)
  (funcall 'markdown-wrap-code-generic)
  (insert "clojure")
  (search-forward "```" nil :noerror))


(defvar markdown-stored-links nil
  "Stores markdown links as (label file heading-text)")

;;;###autoload
(defun markdown-store-link ()
  "Store current markdown heading as a link."
  (interactive)
  (save-excursion
    (unless (markdown-heading-at-point)
      (markdown-back-to-heading))
    (let* ((heading-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
           (heading-text (replace-regexp-in-string "^#+ " "" heading-line))
           (file-path (buffer-file-name))
           (label (format "%s#%s"
                          (file-name-nondirectory file-path)
                          (replace-regexp-in-string " " "-" (downcase heading-text)))))
      (add-to-list 'markdown-stored-links (cons label (list file-path heading-text)))
      (message "Stored link to: %s" label))))

;;;###autoload
(defun markdown-insert-link+ ()
  (interactive)
  (if (not (seq-empty-p markdown-stored-links))
      (let* ((sel (thread-first
                    (completing-read
                     "Heading: "
                     markdown-stored-links
                     nil :require-match)
                    (assoc markdown-stored-links)))
             (file-path (nth 1 sel))
             (heading-text (nth 2 sel))
             (lbl (replace-regexp-in-string " " "-" (downcase heading-text)))
             (label (if (equal (file-truename file-path)
                               (file-truename (buffer-file-name)))
                        (format "#%s" lbl)
                      (format "%s#%s" (file-relative-name file-path) lbl))))
        (markdown-insert-inline-link heading-text label))))
