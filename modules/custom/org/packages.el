;; -*- no-byte-compile: t; -*-
;;; custom/org/packages.el

;; copied directly from the official module. unpinned
(package! org
  :recipe (:host github
           ;; REVIEW I intentionally avoid git.savannah.gnu.org because of SSL
           ;;   issues (see #5655), uptime issues, download time, and lack of
           ;;   shallow clone support.
           :repo "bzg/org-mode"
           :files (:defaults "etc")
           :depth 1
           ;; HACK Org requires a post-install compilation step to generate a
           ;;   org-version.el with org-release and org-git-version functions,
           ;;   using a 'git describe ...' call.  This won't work in a sparse
           ;;   clone and I value smaller network burdens on users over
           ;;   non-essential variables so we fake it:
           :build t
           :pre-build
           (with-temp-file "org-version.el"
             (let ((version
                    (with-temp-buffer
                      (insert-file-contents (doom-path "lisp/org.el") nil 0 1024)
                      (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                          (match-string-no-properties 1)
                        "Unknown"))))
               (insert (format "(defun org-release () %S)\n" version)
                       (format "(defun org-git-version (&rest _) \"%s-??-%s\")\n"
                               version (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                       "(provide 'org-version)\n")))))
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib"))

(package! ox-clip)
(package! org-cliplink)
(package! evil-org :recipe (:host github :repo "hlissner/evil-org-mode"))
(package! org-pdftools)

(package! orgit)
(package! orgit-forge)
(package! org-download)
(package! org-appear)
(package! org-superstar)

(package! org-roam :recipe (:host github :repo "org-roam/org-roam" :files ("*.el" "extensions")))
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! ob-async)
(package! org-edit-indirect :recipe (:host github :repo "agzam/org-edit-indirect.el"))

(package! ob-restclient)

(package! consult-org-roam
  :recipe (:host github
           :repo "jgru/consult-org-roam"))
