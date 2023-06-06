;;; custom/general/autoload/url.el -*- lexical-binding: t; -*-

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
(defun +link-markdown->link-org-mode ()
  "Copies markdown link at point converting it to org-mode format."
  (interactive)
  (when (markdown-link-p)
    (let* ((l (markdown-link-at-pos (point)))
           (desc (nth 2 l))
           (url (nth 3 l)))
      (with-temp-buffer
        (org-insert-link nil url desc)
        (message (buffer-string))
        (kill-new (buffer-string))))))

;;;###autoload
(defun +link-markdown->link-bug-reference ()
  "Copies markdown link at point converting it to a bug-reference format."
  (interactive)
  (when-let ((_ (and
                 (org-in-regexp "\\[.+\\]\\(([^)]+)\\)\\|<\\([^>]+\\)>" 1)
                 (string-match-p "https://github.com"
                                 (or (match-string-no-properties 1)
                                     (match-string-no-properties 2)))))
             (url (funcall 'markdown-link-url)))
    (let* ((issue? (string-match-p "\\/issues\\/" url))
           (pr? (string-match-p "\\/pull\\/" url))
           (bug-ref (when (or issue? pr?)
                      (let-plist (bisect-github-url url)
                        (format "%s %s/%s#%s" (if issue? "Bug" "PR") .org .repo (if issue? .issue .pull))))))
      (kill-new bug-ref)
      (message bug-ref))))

;;;###autoload
(defun +link-markdown->link-plain ()
  "Copies markdown link at point converting it to a plain http format."
  (interactive)
  (when (markdown-link-p)
    (let* ((l (markdown-link-at-pos (point)))
           (url (nth 3 l)))
      (message url)
      (kill-new url))))

;;;###autoload
(defun +link-org->link-markdown ()
  (interactive)
  (let* ((ctx (org-in-regexp org-any-link-re))
         (beg (car ctx)) (end (cdr ctx))
         (link-txt (buffer-substring-no-properties beg end))
         (parsed (unless (string-blank-p link-txt)
                   (seq-map
                    ;; escape square brackets and parens, see:
                    ;; https://emacs.stackexchange.com/questions/68814/escape-all-square-brackets-with-replace-regexp-in-string
                    (lambda (m)
                      (when m
                        (replace-regexp-in-string "\\[\\|\\]\\|(\\|)" "\\\\\\&" m)))
                    (org-link-parse link-txt)))))
    (when parsed
      (let ((md-link (if (eq 1 (length parsed))
                         (format "<%s>" (car parsed))
                       (apply 'format "[%s](%s)" (reverse parsed)))))
        (kill-new md-link)
        (message md-link)))))

;;;###autoload
(defun +link-org->link-bug-reference ()
  (interactive)
  (let* ((ctx (org-in-regexp org-any-link-re))
         (beg (car ctx)) (end (cdr ctx))
         (link-txt (buffer-substring beg end))
         (parsed (unless (string-blank-p link-txt)
                   (seq-map
                    ;; escape square brackets and parens, see:
                    ;; https://emacs.stackexchange.com/questions/68814/escape-all-square-brackets-with-replace-regexp-in-string
                    (lambda (m)
                      (when m
                        (replace-regexp-in-string "\\[\\|\\]\\|(\\|)" "\\\\\\&" m)))
                    (org-link-parse link-txt))))
         (bug-ref (let-plist (bisect-github-url (substring-no-properties (car parsed)))
                    (format "%s %s/%s#%s" (if .pull "PR" "Bug") .org .repo (if .pull .pull .issue)))))
    (delete-region beg end)
    (insert bug-ref)))

;;;###autoload
(defun +link-org->link-plain ()
  (interactive)
  (let* ((ctx (org-in-regexp org-any-link-re))
         (beg (car ctx)) (end (cdr ctx))
         (link-txt (buffer-substring beg end))
         (parsed (unless (string-blank-p link-txt)
                   (seq-map
                    ;; escape square brackets and parens, see:
                    ;; https://emacs.stackexchange.com/questions/68814/escape-all-square-brackets-with-replace-regexp-in-string
                    (lambda (m)
                      (replace-regexp-in-string "\\[\\|\\]\\|(\\|)" "\\\\\\&" m))
                    (org-link-parse link-txt)))))
    (when parsed
      (let ((plain-link (format "%s" (substring-no-properties
                                      (car parsed)))))
        (kill-new plain-link)
        (message plain-link)))))

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
  (when-let* ((o (car (overlays-at (point))))
              (url (overlay-get o 'bug-reference-url))
              (link (let-plist (bisect-github-url url)
                      (format "[%s %s/%s#%s][%s]" (if .issue "Issue" "PR")
                              .org .repo (if .issue .issue .pull) url))))
    (kill-new link)
    (message link)))

;;;###autoload
(defun +link-bug-reference->link-markdown ()
  (interactive)
  (when-let* ((o (car (overlays-at (point))))
              (url (overlay-get o 'bug-reference-url))
              (link (let-plist (bisect-github-url url)
                      (format "[%s %s/%s#%s](%s)" (if .issue "Issue" "PR")
                              .org .repo (if .issue .issue .pull) url))))
    (kill-new link)
    (message link)))

;;;###autoload
(defun +link-bug-reference->link-plain ()
  (interactive)
  (when-let* ((o (car (overlays-at (point))))
              (url (overlay-get o 'bug-reference-url)))
    (kill-new url)
    (message url)))

;;;###autoload
(defun +link-plain->link-org-mode ()
  "Take a URL and convert it to bug-reference type."
  (interactive)
  (when-let* ((url (or (thing-at-point-url-at-point) ""))
              (bounds (bounds-of-thing-at-point 'url))
              ;; TODO: do the GitHub stuff - ticket description, etc.
              (link (if-let ((title (or (org-cliplink-retrieve-title-synchronously url)
                                        (get-gh-item-title url))))
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
         (link (cond
                ((string-match-p ".*https://github.com.*" url)
                 (let-plist (bisect-github-url url)
                   (format "[%s %s/%s#%s](%s)" (if .issue "Issue" "PR")
                           .org .repo (if .issue .issue .pull) url)))

                (t (if-let ((title (org-cliplink-retrieve-title-synchronously url)))
                       (format "[%s](%s)" title url)
                     (format "<%s>" url))))))
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


;; (print
;;  (deferred:sync!
;;   (+find-related-pages "agzam/spacehammer")))
