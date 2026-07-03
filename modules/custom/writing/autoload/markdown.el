;;; custom/writing/autoload/markdown.el -*- lexical-binding: t; -*-

(defvar yank-remember--last nil
  "Cons of (TEXT . FORMAT) for the most recent kill from a markup buffer.
Fallback for when the `yank-source-format' text property is lost,
e.g. after a system clipboard round-trip.")

(defvar paste-convert--in-flight nil
  "Non-nil while `paste-maybe-convert-a' is running.
Stops the nested `evil-visual-paste' round-trip from re-deciding.")

;;;###autoload
(defun buffer-markup-format ()
  "Prisma format symbol for the current buffer, nil for other modes."
  (cond ((derived-mode-p 'gfm-mode 'markdown-mode) 'markdown)
        ((derived-mode-p 'org-mode) 'org)))

;;;###autoload
(defun paste-convert-kill (text source target)
  "Convert kill TEXT from SOURCE to TARGET format, keeping line-wise semantics."
  (require 'prisma)
  (let ((handler (get-text-property 0 'yank-handler text))
        (converted (prisma-render
                    target
                    (prisma-parse source (substring-no-properties text)))))
    (when (eq (car-safe handler) 'evil-yank-line-handler)
      (unless (string-suffix-p "\n" converted)
        (setq converted (concat converted "\n")))
      (put-text-property 0 (length converted) 'yank-handler handler converted))
    converted))

;;;###autoload
(defun yank-remember-format-a (_beg _end &optional _type register _yank-handler)
  "Tag the fresh kill with this buffer's markup format, skipping REGISTER ?_.
`:after' advice for `evil-yank'."
  (when-let* (((not (eq register ?_)))
              (fmt (buffer-markup-format))
              (text (car kill-ring)))
    (put-text-property 0 (length text) 'yank-source-format fmt text)
    (setq yank-remember--last (cons (substring-no-properties text) fmt))))

;;;###autoload
(defun paste-maybe-convert-a (orig-fn count &optional register yank-handler)
  "Cross-convert markdown<->org kills at paste time.
Bare \\[universal-argument] COUNT pastes verbatim.  REGISTER and
YANK-HANDLER pass through to ORIG-FN untouched.
`:around' advice for `evil-paste-after' and `evil-paste-before'."
  (if paste-convert--in-flight
      (funcall orig-fn count register yank-handler)
    (let* ((verbatim (equal count '(4)))
           (count (if verbatim nil count))
           (paste-convert--in-flight t)
           (text (unless register (ignore-errors (current-kill 0))))
           (handler (and (stringp text)
                         (car-safe (get-text-property 0 'yank-handler text))))
           (source (and (stringp text)
                        ;; rectangle pastes carry line data in the handler
                        (not (eq handler 'evil-yank-block-handler))
                        (or (get-text-property 0 'yank-source-format text)
                            (and (equal (substring-no-properties text)
                                        (car yank-remember--last))
                                 (cdr yank-remember--last)))))
           (target (buffer-markup-format))
           (converted
            (when (and (not verbatim) source target (not (eq source target)))
              (condition-case err
                  (paste-convert-kill text source target)
                (error
                 (message "paste conversion failed, pasting as-is (%s)"
                          (error-message-string err))
                 nil)))))
      (if (not converted)
          (funcall orig-fn count register yank-handler)
        (let ((real-ck (symbol-function 'current-kill)))
          (cl-letf (((symbol-function 'current-kill)
                     (lambda (n &optional do-not-move)
                       (if (zerop n)
                           converted
                         (funcall real-ck n do-not-move)))))
            (funcall orig-fn count register yank-handler)))))))

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
