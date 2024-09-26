;;; custom/general/autoload/url.el -*- lexical-binding: t; -*-
(require 'ghub)

;;;###autoload
(defun +url-get-link-type ()
  "For url at point, returns its type and position.
Type can be: org-mode, markdown, plain,or bug-reference.
Position is a cons-cell."
  (require 'org)
  (require 'bug-reference)
  (cond
   ((when-let ((pos (org-in-regexp org-link-bracket-re 1))
               (_ (string-match-p "^http\\|https" (match-string-no-properties 1))))
      (cons 'org-mode pos)))

   ((when-let ((pos (org-in-regexp "<\\(https?://[^>]+\\)>\\|\\[\\([^]]+\\)\\]\\((https?://[^)]+)\\)" 1)))
      (cons 'markdown pos)))

   ((when-let ((pos (org-in-regexp "^http\\|https" 1)))
      (cons 'plain pos)))

   ((when-let ((pos (org-in-regexp bug-reference-bug-regexp  1)))
      (cons 'bug-reference pos)))))

;;;###autoload
(defun +embark-target-markdown-link-at-point ()
  "Target markdown link at point"
  (when-let* ((type+pos (+url-get-link-type))
              (markdown? (eq 'markdown (car type+pos)))
              (beg (cadr type+pos))
              (end (cddr type+pos)))
    `(markdown-link ,(buffer-substring-no-properties beg end) . ,(cons beg end))))

;;;###autoload
(defun +embark-target-bug-reference-link-at-point ()
  "Target bug-reference link at point"
  (when-let* ((type+pos (+url-get-link-type))
              (bug-ref? (eq 'bug-reference (car type+pos)))
              (beg (cadr type+pos))
              (end (cddr type+pos)))
    `(bug-reference-link ,(buffer-substring-no-properties beg end) . ,(cons beg end))))

;;;###autoload
(defun +embark-target-RFC-number-at-point ()
  "Target RFC number at point.
anything like: RFC 123, rfc-123, RFC123 or rfc123."
  (when-let* ((rfc-pattern "\\b[rR][fF][cC][- ]?[0-9]+\\b")
              (bounds (org-in-regexp rfc-pattern 1))
              (beg (car bounds))
              (end (cdr bounds)))
    `(rfc-number ,(buffer-substring-no-properties beg end)
      . ,(cons beg end))))

;;;###autoload
(defun +link-markdown->link-org-mode ()
  "Copies markdown link at point converting it to org-mode format."
  (interactive)
  (when-let* ((ref (+embark-target-markdown-link-at-point))
              (bounds (nthcdr 2 ref))
              (url (nth 3 (markdown-link-at-pos (point))))
              (label (nth 2 (markdown-link-at-pos (point))))
              (link (format "[[%s][%s]]" url
                            (if label label
                              (or (get-gh-item-title url)
                                  (org-cliplink-retrieve-title-synchronously url))))))
    (delete-region (car bounds) (cdr bounds))
    (insert link)))

;;;###autoload
(defun +link-markdown->link-bug-reference ()
  "Copies markdown link at point converting it to a bug-reference format."
  (interactive)
  (when-let* ((ref (+embark-target-markdown-link-at-point))
              (bounds (nthcdr 2 ref))
              (url (nth 3 (markdown-link-at-pos (point))))
              (bug-ref (let-plist (bisect-github-url url)
                         (format "%s/%s#%s" .org .repo (or .pull .issue)))))
    (delete-region (car bounds) (cdr bounds))
    (insert bug-ref)))

;;;###autoload
(defun +link-markdown->link-plain ()
  "Copies markdown link at point converting it to a plain http format."
  (interactive)
  (when-let* ((ref (+embark-target-markdown-link-at-point))
              (bounds (nthcdr 2 ref))
              (url (nth 3 (markdown-link-at-pos (point)))))
    (delete-region (car bounds) (cdr bounds))
    (insert url)))

;;;###autoload
(defun +link-markdown->just-text ()
  "Convert markdown link to simple text."
  (interactive)
  (when-let* ((ref (+embark-target-markdown-link-at-point))
              (bounds (nthcdr 2 ref))
              (desc (nth 2 (markdown-link-at-pos (point)))))
    (delete-region (car bounds) (cdr bounds))
    (insert desc)))

;;;###autoload
(defun +link-org->link-markdown ()
  (interactive)
  (let* ((ctx (org-element-context))
         (ctx (org-element-lineage ctx '(link) t))
         (beg (org-element-property :begin ctx))
         (end (org-element-property :end ctx))
         (url (org-element-property :raw-link ctx))
         (begd (org-element-property :contents-begin ctx))
         (endd (org-element-property :contents-end ctx))
         (desc (if (and begd endd (not (= begd endd)))
                   (replace-regexp-in-string
                    "[ \n]+" " "
                    (string-trim
                     (buffer-substring-no-properties begd endd)))))
         (desc (or desc (or (get-gh-item-title url)
                            (org-cliplink-retrieve-title-synchronously url))))
         (link (format "[%s](%s)" desc url)))
    (when (and link desc)
      (delete-region beg end)
      (insert link))))

;;;###autoload
(defun +link-org->link-bug-reference ()
  (interactive)
  (when-let* ((ctx (org-element-context))
              (ctx (org-element-lineage ctx '(link) t))
              (beg (org-element-property :begin ctx))
              (end (org-element-property :end ctx))
              (url (org-element-property :raw-link ctx))
              (bug-ref (let-plist (bisect-github-url url)
                         (format "%s/%s#%s" .org .repo (or .pull .issue)))))
    (delete-region beg end)
    (insert bug-ref)))

;;;###autoload
(defun +link-org->link-plain ()
  (interactive)
  (when-let* ((ctx (org-element-context))
              (ctx (org-element-lineage ctx '(link) t))
              (beg (org-element-property :begin ctx))
              (end (org-element-property :end ctx))
              (url (org-element-property :raw-link ctx)))
    (save-excursion
      (delete-region beg end)
      (insert url))))

(defun +link-org->just-text ()
  "Convert link to simple text."
  (interactive)
  (when-let* ((ctx (org-element-context))
              (ctx (org-element-lineage ctx '(link) t))
              (beg (org-element-property :begin ctx))
              (end (org-element-property :end ctx))
              (begd (org-element-property :contents-begin ctx))
              (endd (org-element-property :contents-end ctx))
              (desc (unless (= begd endd)
                      (replace-regexp-in-string
                       "[ \n]+" " "
                       (string-trim
                        (buffer-substring-no-properties begd endd))))))
    (delete-region beg end)
    (insert desc)))

;;;###autoload
(defun +link-bug-reference->link-org-mode ()
  (interactive)
  (when-let* ((ref (embark-target-bug-reference-at-point))
              (url (nth 1 ref))
              (bounds (nthcdr 2 ref))
              (link (format "[[%s][%s]]"
                            url
                            (or (get-gh-item-title url)
                                (org-cliplink-retrieve-title-synchronously url)))))
    (delete-region (car bounds) (cdr bounds))
    (insert link)))

;;;###autoload
(defun +link-bug-reference->link-markdown ()
  (interactive)
  (when-let* ((ref (embark-target-bug-reference-at-point))
              (url (nth 1 ref))
              (bounds (nthcdr 2 ref))
              (link (let-plist (bisect-github-url url)
                      (format "[%s](%s)"
                              (get-gh-item-title url)
                              url))))
    (delete-region (car bounds) (cdr bounds))
    (insert link)))

;;;###autoload
(defun +link-bug-reference->link-plain ()
  (interactive)
  (when-let* ((ref (embark-target-bug-reference-at-point))
              (url (nth 1 ref))
              (bounds (nthcdr 2 ref)))
    (delete-region (car bounds)
                   (cdr bounds))
    (insert url)))

;;;###autoload
(defun +link-plain->link-org-mode ()
  "Take a URL and convert it to bug-reference type."
  (interactive)
  (when-let* ((url (or (thing-at-point-url-at-point) ""))
              (bounds (bounds-of-thing-at-point 'url))
              (gh-title (get-gh-item-title url))
              (link (if-let ((title (if (or (not gh-title) (string= gh-title url))
                                        (org-cliplink-retrieve-title-synchronously url)
                                      gh-title)))
                        (format "[[%s][%s]]" url title)
                      (format "[[%s]]" url))))
    (delete-region (car bounds) (cdr bounds))
    (insert link)))

;;;###autoload
(defun +link-plain->link-markdown ()
  "Take a URL and convert it to markdown type."
  (interactive)
  (let* ((url (or (thing-at-point-url-at-point) ""))
         (bounds (bounds-of-thing-at-point 'url))
         (link (if-let ((title (or (get-gh-item-title url)
                                   (org-cliplink-retrieve-title-synchronously url))))
                   (format "[%s](%s)" title url)
                 (format "<%s>" url))))
    (delete-region (car bounds) (cdr bounds))
    (insert link)))

;;;###autoload
(defun +link-plain->link-bug-reference ()
  "Take a URL and convert it to bug-reference type."
  (interactive)
  (when-let* ((url (or (thing-at-point-url-at-point) ""))
              (bounds (bounds-of-thing-at-point 'url))
              (link (cond
                     ((string-match-p ".*https://github.com.*" url)
                      (let-plist (bisect-github-url url)
                        (format "%s %s/%s#%s" (if .pull "PR" "Bug")
                                .org .repo (if .pull .pull .issue)))))))
    (delete-region (car bounds) (cdr bounds))
    (insert link)))

;;;###autoload
(defun +open-link-in-vlc ()
  "Open link at point in VLC player."
  (interactive)
  (when-let* ((ctx (org-element-context))
              (path (org-link-unescape
                     (org-element-property :path ctx))))
    ;; TODO: Add Linux version
    (let ((dir? (file-directory-p path)))
      (shell-command
       (concat "open -a VLC \"" path "\""
               (when dir? " --args --playlist-autostart"))))))

;;;###autoload
(defun +find-related-pages (term)
  "Finds all mentions of a given TERM in various websites.
Uses bing api. Needs api-key stored in auth-source file.
Returns list of org-mode links."
  (let* ((sites '("news.ycombinator.com"
                  "lobste.rs"
                  "reddit.com"
                  "youtube.com"
                  "github.com"))
         (api-key (auth-source-pick-first-password
                   :host "api.bing.microsoft.com"))
         ;; need to send multiple requests,
         ;; stupid bing can't handle more than one site:x
         ;; clause in a single query
         (reqs
          (deferred:parallel-list
           (cl-loop
            for site in sites
            collect
            ((lambda (uri)
               (let ((query (format "\"%s\" site:%s" term uri)))
                 (deferred:$
                  (request-deferred
                   (concat "https://api.bing.microsoft.com/v7.0/search")
                   :params `(("mkt" . "en-US")
                             ("q" . ,query))
                   :headers `(("Ocp-Apim-Subscription-Key" . ,api-key))
                   :parser 'json-read)
                  (deferred:nextc
                   it
                   (lambda (resp)
                     (->>
                      resp
                      (request-response-data)
                      (funcall (-rpartial #'a-get-in '(webPages value)))
                      (-map (lambda (x)
                              (let-alist x
                                (format "[[%s][%s]]" .url .name))))))))))
             site)))))
    (deferred:$
     reqs
     (deferred:nextc
      it
      (lambda (x)
        (->> x
             (-flatten)
             (-remove #'null)))))))

;;;###autoload
(defun +find-related-pages-serpapi (&optional url)
  "Using serpapi.com finds pages linking to URL on various sites.
Returns Org-mode buffer with links."
  (interactive)
  (let* ((url (if (region-active-p)
                  (buffer-substring
                   (region-beginning)
                   (region-end))
                url))
         (url (or url (thing-at-point-url-at-point)
                  (read-string "Enter URL: ")))
         (sites '("news.ycombinator.com"
                  "lobste.rs"
                  "reddit.com"
                  "youtube.com"
                  "github.com"))
         (api-key (auth-source-pick-first-password :host "serpapi.com"))
         (query (thread-last
                  sites
                  (-map (lambda (x)
                          (format "site:%s" x)))
                  (-interpose " OR ")
                  (apply 'concat)
                  (concat "link:" url " ")
                  url-hexify-string))
         (req-url (format "https://serpapi.com/search?api_key=%s&q=%s" api-key query))
         (pipeline (deferred:$
                    (request-deferred
                     req-url
                     :parser 'json-read)
                    (deferred:nextc
                     it
                     (lambda (resp)
                       (thread-last
                         resp
                         (request-response-data)
                         (alist-get 'organic_results)
                         (-map (lambda (x)
                                 (let-alist x
                                   (format "[[%s][%s]]" .link .title)))))))))
         (links (deferred:sync! pipeline))
         (buf (generate-new-buffer (format "* links to %s *" url))))
    (with-current-buffer buf
      (dolist (link links)
        (insert link)
        (insert "\n"))
      (org-mode)
      (if (called-interactively-p 'interactive)
          (pop-to-buffer buf)
        buf))))
