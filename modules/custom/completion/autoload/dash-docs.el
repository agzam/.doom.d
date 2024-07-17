;;; custom/completion/autoload/dash-docs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +browse-dash-doc (url)
  "Open Dash doc in eww."
  (+eww-open-in-other-window url)
  ;; make it readable
  (run-with-timer
   0.2 nil
   (lambda (url)
     (when-let ((eww-buf
                 (seq-find
                  (lambda (b)
                    (and (eq 'eww-mode
                             (buffer-local-value
                              'major-mode b))
                         (string=
                          url
                          (plist-get
                           (buffer-local-value
                            'eww-data b)
                           :url))))
                  (buffer-list))))
       (with-current-buffer eww-buf
         (when-let* ((url (plist-get (buffer-local-value 'eww-data eww-buf) :url))
                     (clojure? (string-match-p "ClojureDocs" url)))
           (clojure-mode))
         ;; (eww-readable)
         (pop-to-buffer eww-buf))))
   url))

;;;###autoload
(defun +consult-dash-doc (term)
  (condition-case err
      (if (and (featurep 'cider)
               (cider-connected-p))
          (cider-clojuredocs-lookup
           (cider-fqn-symbol-at-point))
        (consult-dash term))
    (error
     (if (string-match-p
          "Wrong type argument: .*"
          (error-message-string err))
         (lsp-signature-toggle-full-docs)))))
