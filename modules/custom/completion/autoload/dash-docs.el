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

;; Version-aware installs: skip the (.tgz) download when the locally
;; recorded version already matches the source, so reruns stay cheap.

(defun dash-docs--version-file ()
  "Return the path of the docset version registry."
  (expand-file-name ".dash-versions.eld" (dash-docs-docsets-path)))

(defun dash-docs--read-versions ()
  "Return the alist of recorded docset versions, or nil."
  (let ((file (dash-docs--version-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (ignore-errors (read (current-buffer)))))))

(defun dash-docs--installed-version (name)
  "Return the recorded version for docset NAME, or nil.
NAME is matched case-insensitively."
  (cdr (cl-assoc name (dash-docs--read-versions)
                 :test #'string-equal-ignore-case)))

(defun dash-docs--record-version (name version)
  "Record VERSION as the installed version for docset NAME."
  (let ((versions (cons (cons name version)
                        (cl-remove name (dash-docs--read-versions)
                                   :key #'car
                                   :test #'string-equal-ignore-case))))
    (with-temp-file (dash-docs--version-file)
      (prin1 versions (current-buffer)))))

(defun dash-docs--up-to-date-p (name version)
  "Return non-nil when docset NAME is already installed at VERSION.
Both a matching recorded version and an on-disk bundle are required, so a
manually deleted docset is reinstalled even if its marker lingers."
  (and version
       (equal version (dash-docs--installed-version name))
       (cl-member name (dash-docs-installed-docsets)
                  :test #'string-equal-ignore-case)))

(defun dash-docs--feed-version (feed-path)
  "Return the version string from the docset XML feed at FEED-PATH, or nil."
  (let* ((xml (xml-parse-file feed-path))
         (version (xml-get-children (car xml) 'version)))
    (and version (cl-caddr (cl-first version)))))

;;;###autoload
(defun dash-docs-install-user-docset+ (&optional docset)
  "Install unofficial docset DOCSET, skipping it when already up to date.
Without DOCSET, prompt for one.  The feed version is compared against the
local registry and the archive is downloaded only when it changed."
  (interactive)
  (let* ((docsets (dash-docs-unofficial-docsets))
         (docset-name (or docset
                          (dash-docs-read-docset
                           "Install docset"
                           (mapcar 'car docsets))))
         (docset (assoc-default docset-name docsets))
         (version (caddr docset)))
    (when (dash-docs--ensure-created-docsets-path (dash-docs-docsets-path))
      (if (dash-docs--up-to-date-p docset-name version)
          (message "dash-docs: %s up to date (%s), skipping" docset-name version)
        (let ((url
               (format "https://kapeli.com/feeds/zzz/user_contributed/build/%s/%s"
                       (car docset)
                       (cadr docset))))
          (dash-docs--install-docset url (car docset))
          (dash-docs--record-version docset-name version))))))

;;;###autoload
(defun dash-docs-install-docset+ (docset-name)
  "Install official docset DOCSET-NAME, skipping it when already up to date.
Only the small feed XML is fetched to compare its version against the local
registry; the docset archive is downloaded only when it changed."
  (interactive (list (dash-docs-read-docset
                      "Install docset"
                      (dash-docs-official-docsets))))
  (when (dash-docs--ensure-created-docsets-path (dash-docs-docsets-path))
    (let ((feed-url (format "%s/%s.xml" dash-docs-docsets-url docset-name))
          (feed-tmp-path (format "%s%s-feed.xml" temporary-file-directory docset-name)))
      (dash-docs-with-emacs-bug-workaround
       (url-copy-file feed-url feed-tmp-path t))
      (let ((version (dash-docs--feed-version feed-tmp-path)))
        (if (dash-docs--up-to-date-p docset-name version)
            (message "dash-docs: %s up to date (%s), skipping" docset-name version)
          (dash-docs--install-docset (dash-docs-get-docset-url feed-tmp-path)
                                     docset-name)
          (dash-docs--record-version docset-name version))))))

;;;###autoload
(defun dash-docs-unofficial-docsets+ ()
  "Return user-contributed docsets as a list of entries.
Each entry is (NAME SLUG ARCHIVE VERSION); VERSION drives the update check."
  (let ((user-docs (assoc-default 'docsets
                                  (dash-docs-read-json-from-url
                                   "https://kapeli.com/feeds/zzz/user_contributed/build/index.json"))))
    (mapcar (lambda (docset)
              (list
               (assoc-default 'name docset)
               (car docset)
               (assoc-default 'archive docset)
               (assoc-default 'version docset)))
            user-docs)))
