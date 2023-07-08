;; -*- no-byte-compile: t; -*-
;;; custom/org/packages.el

;; copied directly from the official module. unpinned
(package! org
  :pin "ca873f7fe47546bca19821f1578a6ab95bf5351c"
  :recipe (:host github
           ;; REVIEW: I intentionally avoid git.savannah.gnu.org because of SSL
           ;;   issues (see #5655), uptime issues, download time, and lack of
           ;;   shallow clone support.
           :repo "emacs-straight/org-mode"
           :files (:defaults "etc")
           :depth 1
           ;; HACK: Org has a post-install step that generates org-version.el
           ;;   and org-loaddefs.el, but Straight doesn't invoke this step, and
           ;;   the former doesn't work if the Org repo is a shallow clone.
           ;;   Rather than impose the network burden of a full clone (and other
           ;;   redundant work in Org's makefile), I'd rather fake these files
           ;;   instead. Besides, Straight already produces a org-autoloads.el,
           ;;   so org-loaddefs.el isn't needed.
           :build t
           :pre-build
           (progn
             (with-temp-file "org-loaddefs.el")
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
                         "(provide 'org-version)\n"))))))

(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib"))

(package! ox-clip)
(package! org-cliplink)
(package! evil-org :recipe (:host github :repo "hlissner/evil-org-mode"))

(package! orgit)
(package! orgit-forge)
(package! org-download)
(package! org-appear :recipe (:host github :repo "awth13/org-appear"))
(package! org-superstar)

(package! org-roam :recipe (:host github :repo "org-roam/org-roam" :files ("*.el" "extensions")))
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! ob-async)
(package! org-edit-indirect :recipe (:host github :repo "agzam/org-edit-indirect.el"))

;; (package! ob-restclient)
(package! ob-http)
(package! ox-gfm)

(package! consult-org-roam :recipe (:host github :repo "jgru/consult-org-roam"))

(package! org-pomodoro)
(package! verb :recipe (:host github :repo "agzam/verb"
                        :branch "development"))

(package! anki-editor)

(package! org-transclusion :recipe (:host github :repo "nobiot/org-transclusion"))
(package! toc-org)
