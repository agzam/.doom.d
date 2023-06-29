;;; custom/git/autoload/misc.el -*- lexical-binding: t; -*-

;; Shared by eleven_cupfuls on Reddit
;; https://www.reddit.com/r/emacs/comments/w9p2oo/resetting_dirlocalsel/ii2kn47/

;;;###autoload
(defun +reload-dir-locals (proj)
  "Read values from the current project's .dir-locals file and
apply them in all project file buffers as if opening those files
for the first time.

Signals an error if there is no current project."
  (interactive (list (project-current)))
  (unless proj
    (user-error "There doesn't seem to be a project here"))
  ;; Load the variables; they are stored buffer-locally, so...
  (hack-dir-local-variables)
  ;; Hold onto them...
  (let ((locals dir-local-variables-alist))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (equal proj (project-current))
                   buffer-file-name)
          ;; transfer the loaded values to this buffer...
          (setq-local dir-local-variables-alist locals)
          ;; and apply them.
          (hack-local-variables-apply))))))

(defun +decrypt-gh-token ()
  "Retrieves encrypted GitHub token from auth-sources."
  (funcall
   (plist-get
    (car
     (auth-source-search :machine "api.github.com"
                         :type 'netrc
                         :max 1))
    :secret)))

(defun bisect-github-url (url)
  "Returns plist with parts of GitHub URL."
  ;; different kinds of GH links, for future reference:
  ;;
  ;; plain:      https://github.com/fniessen/refcard-org-mode
  ;; issues:     https://github.com/fniessen/refcard-org-mode/issues
  ;; PRs:        https://github.com/fniessen/refcard-org-mode/pulls
  ;; an issue:   https://github.com/fniessen/refcard-org-mode/issues/7
  ;; a PR:       https://github.com/fniessen/refcard-org-mode/pull/5
  ;; a file:     https://github.com/fniessen/refcard-org-mode/blob/master/images/org-mode-unicorn.png
  ;; a diff:     https://github.com/advthreat/iroh/pull/7317/files#diff-3edf99653c7afaf324e036f0df597c2322e4fdfbb0f266bc6370f747e1e51bb4
  ;; comparison: https://github.com/advthreat/iroh/compare/master...1.78-proposal
  ;;
  (let* ((seg "\\([A-z,0-9,._~!$&'()*+,;=:@%-]+\\)")
         (bare-rx (concat "\\(https\\:\\/\\/github.com\\)\\/" seg "\\/" seg))
         (file-rx (concat bare-rx "\\/blob\\/" seg "\\/\\(.*\\)"))
         (issue-rx (concat "\\(https\\:\\/\\/github.com\\)\\/" seg
                           "\\/" seg "\\/issues\\/\\([0-9]+\\)" ))
         (pr-rx (concat "\\(https\\:\\/\\/github.com\\)\\/" seg
                        "\\/" seg "\\/pull\\/\\([0-9]+\\)"))
         (type (if (not (string-match-p bare-rx url))
                   (error "Is that a GitHub url?\n%s" url)
                 (cond ((string-match file-rx url) 'file)
                       ((string-match issue-rx url) 'issue)
                       ((string-match pr-rx url) 'pull)
                       ((string-match bare-rx url) 'bare)))))
    (list :org (match-string 2 url)
          :repo (match-string 3 url)
          :ref (when (eq type 'file)
                 (match-string 4 url))
          :issue (when (eq type 'issue)
                   (match-string 4 url))
          :pull (when (eq type 'pull)
                  (match-string 4 url))
          :path (match-string 5 url)
          :ext (ignore-errors
                 (replace-regexp-in-string
                  ".*\\." "" (match-string 5 url))))))

;;;###autoload
(defun +fetch-github-raw-file (url)
  "Open the raw file of a GitHub URL.
If URL is a link to a file, it extracts its raw form and tries to open in a buffer."
  (let* ((parts (bisect-github-url url))
         (raw-url (thread-last
                    url
                    (replace-regexp-in-string
                     "https://github.com/"
                     "https://raw.githubusercontent.com/")
                    (replace-regexp-in-string "blob/" "")))
         (path (plist-get parts :path))
         (bufname (format
                   "%s/%s/%s | %s"
                   (plist-get parts :org)
                   (plist-get parts :repo)
                   (plist-get parts :ref)
                   (plist-get parts :path)))
         (mode (thread-first
                 (concat "." (plist-get parts :ext))
                 (assoc auto-mode-alist #'string-match-p)
                 (cdr))))
    (when path
      (request raw-url
        :sync t
        :headers `(("Authorization" . ,(format "Token %s" (+decrypt-gh-token))))
        :parser 'buffer-string
        :complete (cl-function
                   (lambda (&key data &allow-other-keys)
                     (when data
                       (with-current-buffer (get-buffer-create bufname)
                         (erase-buffer)
                         (insert data)
                         (funcall mode)
                         (pop-to-buffer (current-buffer))))))))))


;;;###autoload
(defun magit-transient-unblock-global-keys ()
  "Enable/unblock <M-x>, <M-:>, <C-h k>, etc. keys in Magit transients."
  (dolist (sfx (transient-suffixes 'magit-dispatch))
    (when-let ((cmd (transient--suffix-command sfx)))
      (ignore-errors
        (transient-append-suffix cmd '(0)
          [:hide always
           :setup-children
           (lambda (_)
             (list
              (transient-parse-suffix
               'magit-remote
               [("M-x" "M-x" execute-extended-command :transient t)
                ("M-:" "M-:" eval-expression :transient t)
                ("C-h k" "describe key"
                 (lambda ()
                   (interactive)
                   (let ((transient-hide-during-minibuffer-read nil))
                     (call-interactively (keymap-global-lookup "C-h k"))))
                 :transient t)])))])))))

;;;###autoload
(defun transient-export-content ()
  "Open content of the current transient in a buffer."
  (interactive)
  (when-let ((trans (get-buffer transient--buffer-name))
             (b (generate-new-buffer
                 (format "%S transient"
                         transient-current-command))))
    (with-current-buffer b
      (insert-buffer-substring-no-properties trans)
      (switch-to-buffer-other-window b))))

;;;###autoload
(defun consult-gh-remove-org+ (x)
  (interactive)
  (setq consult-gh--known-orgs-list
        (cl-delete x consult-gh--known-orgs-list :test #'string=))
  (message "Deleted '%s' org from list of orgs." x))
