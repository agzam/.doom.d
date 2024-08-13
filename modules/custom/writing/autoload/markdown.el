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
             "pandoc --wrap=none -f org -t markdown_strict" nil t)
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
           (content (buffer-substring beg end)))
      (delete-region beg end)
      (deactivate-mark)
      (insert
       (format "<details>\n  <summary></summary>\n%s\n</details>" content))
      (search-backward "<summary>")
      (forward-char 9)
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
