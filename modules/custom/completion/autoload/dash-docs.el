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
          (condition-case nil
              (cider-clojuredocs-lookup
               (cider-fqn-symbol-at-point))
            (user-error (consult-dash term)))
        (consult-dash term))
    (error
     (if (string-match-p
          "Wrong type argument: .*"
          (error-message-string err))
         (lsp-signature-toggle-full-docs)))))

;;;###autoload
(defun dash-docs-activate-docset-a (orig-fn &optional docset)
  (if (and docset (not (cl-member docset (dash-docs-installed-docsets)
                                  :test #'string-equal-ignore-case)))
      (cond ((cl-member docset (mapcar 'car (dash-docs-unofficial-docsets))
                        :test #'string-equal-ignore-case)
             (dash-docs-install-user-docset docset))

          ((cl-member docset (dash-docs-official-docsets)
                      :test #'string-equal-ignore-case)
           (dash-docs-install-docset docset))))
  (funcall orig-fn docset))

;;;###autoload
(defun dash-docs-install-user-docset+ (&optional docset)
  "Download an unofficial docset with specified DOCSET-NAME and
move its stuff to docsets-path."
  (interactive)
  (let* ((docsets (dash-docs-unofficial-docsets))
         (docset-name (or docset
                          (dash-docs-read-docset
                           "Install docset"
                           (mapcar 'car docsets))))
         (docset (assoc-default docset-name docsets)))
    (when (dash-docs--ensure-created-docsets-path (dash-docs-docsets-path))
      (let ((url
             (format "https://kapeli.com/feeds/zzz/user_contributed/build/%s/%s"
                     (car docset)
                     (cadr docset))))
        (dash-docs--install-docset url (car docset))))))

;;;###autoload
(defun dash-docs-unofficial-docsets+ ()
  "Return a list of lists with docsets contributed by users.
The first element is the docset's name second the docset's archive url."
  (let ((user-docs (assoc-default 'docsets
                                  (dash-docs-read-json-from-url
                                   "https://kapeli.com/feeds/zzz/user_contributed/build/index.json"))))
    (mapcar (lambda (docset)
              (list
               (assoc-default 'name docset)
               (car docset)
               (assoc-default 'archive docset)))
            user-docs)))
