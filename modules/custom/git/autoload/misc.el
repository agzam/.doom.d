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

;;;###autoload
(defun parse-git-url (url)
  "Parse git URL."
  (when (string-match "\\(?:git@\\|https?://\\)?\\(?:www\\.\\)?\\([^:/]+\\)[:/]\\([^/]+\\)/\\(.+?\\)\\(?:\\.git\\)?$" url)
    (list :host (match-string 1 url)
          :org (match-string 2 url)
          :repo (match-string 3 url))))

;;;###autoload
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
    (list
     :forge (match-string 1 url)
     :org (match-string 2 url)
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
             ".*\\.\\([^.#]+\\)$" "\\1"
             (match-string 5 url)))
     :line (ignore-errors
             (let* ((fname (match-string 5 url))
                    (re ".*#L\\([0-9]+\\).*"))
               (when (string-match re fname)
                 (replace-regexp-in-string
                  re "\\1" fname)))))))

;;;###autoload
(defun +fetch-github-raw-file (url)
  "Open the raw file of a GitHub URL.
If URL is a link to a file, it extracts its raw form and tries to open in a buffer."
  (interactive)
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
      (let* ((auth-source-debug nil)
             (token (format "Token %s" (auth-host->pass "api.github.com"))))
       (request raw-url
         :sync t
         :headers `(("Authorization" . ,token))
         :parser 'buffer-string
         :complete (cl-function
                    (lambda (&key data &allow-other-keys)
                      (when data
                        (with-current-buffer (get-buffer-create bufname)
                          (erase-buffer)
                          (insert data)
                          (if mode (funcall mode)
                            ;; for files with mode headers
                            (hack-local-variables))
                          (when-let* ((line (plist-get parts :line))
                                      (line-num (1- (string-to-number line))))
                            (goto-char (point-min))
                            (forward-line line-num))
                          (switch-to-buffer-other-window
                           (current-buffer)))))))))))

;;;###autoload
(defun forge-visit-topic-via-url (&optional url)
  "Opens Forge Topic buffer or the raw file, based on GitHub URL."
  (interactive)
  (require 'forge)
  (let* ((url (or url
                  ;; basic
                  (thing-at-point-url-at-point)
                  ;; org-link
                  (thread-last
                    (org-element-lineage (org-element-context) '(link) t)
                    (org-element-property :raw-link))
                  ;; markdown link
                  (nth 3 (markdown-link-at-pos (point)))
                  ;; bug-reference
                  (when-let* ((o (car (overlays-at (point)))))
                    (overlay-get o 'bug-reference-url))))
         (parts (bisect-github-url url))
         (topic-num (when-let ((tn (or (plist-get parts :issue) (plist-get parts :pull)))
                               (string-to-number tn))))
         (owner (plist-get parts :org))
         (repo-name (plist-get parts :repo))
         (ext (plist-get parts :ext)))
    (cond
     (topic-num
      (let* ((repo-url (format "%s/%s/%s" (plist-get parts :forge) owner repo-name))
             (repo (or (ignore-errors (forge-get-repository repo-url t t))
                       (forge-get-repository repo-url nil 'create)))
             ;; let's try getting the topic from the forge db
             (topic
              (or
               ;; This one is tricky:
               ;; First, it tries to find the topic in forge db,
               ;; that may fail if the topic too new and never fetched into the db.
               ;; Then, we try to update the db.
               ;; Since at this point we don't know if it's an issue or PR:
               ;; first we try to find an issue with the topic-num,
               ;; otherwise we try to find the PR
               ;; if found, we update the db and then grab the topic from it.
               ;; Fetching functions are async, have to wrap it into a deferred
               (forge-get-topic repo topic-num)
               (deferred:sync!
                (deferred:$
                 (deferred:next
                  (lambda ()
                    (let* ((d (deferred:new #'identity)))
                      (ghub-fetch-issue
                       owner repo-name topic-num
                       (lambda (issue-data)
                         (forge--update-issue repo issue-data nil)
                         (deferred:callback-post d (forge-get-topic repo topic-num)))
                       nil
                       :errorback
                       (lambda (&rest _args)
                         (ghub-fetch-pullreq
                          owner repo-name topic-num
                          (lambda (pr-data)
                            (forge--update-pullreq repo pr-data nil)
                            (deferred:callback-post d (forge-get-topic repo topic-num)))
                          nil
                          :errorback
                          (lambda (&rest _args)
                            (message "Can't seem to locate issue/PR for %s" url)))))
                      d))))))))

        (cl-letf (;; otherwise it complains for not running inside a git repo
                  ((symbol-function #'magit-toplevel)
                   (lambda () default-directory))
                  (magit-display-buffer-function
                   (lambda (buf)
                     (get-buffer-window
                      (pop-to-buffer buf)))))
          (forge-topic-setup-buffer topic))))

     (ext (+fetch-github-raw-file url)))))

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
(defun straight-install-github-package (&optional url)
  "Installs a package, fetching it from github."
  (interactive)
  (when-let ((name "")
             (gh-repo (cond
                       (url (let-plist
                                (bisect-github-url "https://github.com/agzam/spacehammer")
                              (setq name .repo)
                              (format "%s/%s" .org .repo)))
                       ((featurep 'consult-gh)
                        (let-plist (cdr (consult-gh-search-repos "" :no-action))
                          (setq name .package)
                          .repo))))
             (name (read-from-minibuffer "package name: " name)))
    (straight-use-package
      `(,(intern name) :type git :host github :repo ,gh-repo))))


(defun make-path (&rest parts)
  (expand-file-name (mapconcat #'identity parts "/")))

;;;###autoload
(defun +git-clone ()
  "Simplify cloning using org-name/repo-name dir structure."
  (interactive)
  (let* ((orig-fn (symbol-function 'magit-clone-read-args))
         (adv-fn (lambda (_)
                   (let* ((repo (magit-clone-read-repository))
                          (dir
                           (read-directory-name
                            "Clone to: "
                            (let-plist (parse-git-url repo)
                              (make-path
                               (or magit-clone-default-directory
                                   "~/GitHub/")
                               .org .repo)))))
                     (setf (symbol-function 'magit-clone-read-args)
                          orig-fn)
                     (list repo dir (transient-args 'magit-clone))))))
   (advice-add 'magit-clone-read-args :around adv-fn))
  (funcall-interactively #'magit-clone :transient))

;;;###autoload
(defun update-ssh-auth-sock-h ()
  "Update SSH_AUTH_SOCK environment variable to the current SSH agent socket.
This is useful for long-running Emacs sessions in tmux where the SSH
forwarding socket changes between SSH connections. Only runs when in
terminal mode and connected via SSH."
  (when (and (not (display-graphic-p))
             (getenv "SSH_CONNECTION"))
    (when-let ((sock (car (file-expand-wildcards "/tmp/ssh-*/agent.*" t))))
      (setenv "SSH_AUTH_SOCK" sock))))
