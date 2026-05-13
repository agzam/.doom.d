;;; custom/elisp/autoload/create-package.el -*- lexical-binding: t; -*-

(defvar create-emacs-package--templates-dir
  (expand-file-name "templates"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (or load-file-name buffer-file-name)))))
  "Directory containing package scaffold templates.")

(defun create-emacs-package--substitute (text replacements)
  "Replace placeholder patterns in TEXT using REPLACEMENTS alist.
Each key in REPLACEMENTS is a literal string like \"{{PKG}}\"."
  (let ((result text))
    (dolist (pair replacements result)
      (setq result (replace-regexp-in-string
                    (regexp-quote (car pair)) (cdr pair) result t t)))))

(defun create-emacs-package--write-template (template-name dest replacements)
  "Read TEMPLATE-NAME from templates dir, substitute, write to DEST."
  (let* ((tmpl-file (expand-file-name template-name create-emacs-package--templates-dir))
         (content (with-temp-buffer
                    (insert-file-contents tmpl-file)
                    (buffer-string)))
         (result (create-emacs-package--substitute content replacements)))
    (with-temp-file dest
      (insert result))))

(defun create-emacs-package--download-license (pkg-dir)
  "Download GPL-3.0 license text to PKG-DIR/LICENSE.
Falls back to a stub if the download fails."
  (require 'url)
  (let ((license-file (expand-file-name "LICENSE" pkg-dir)))
    (condition-case err
        (url-copy-file "https://www.gnu.org/licenses/gpl-3.0.txt" license-file)
      (error
       (with-temp-file license-file
         (insert "GPL-3.0-or-later\n\nSee https://www.gnu.org/licenses/gpl-3.0.txt\n"))
       (message "Warning: could not download license: %s" (error-message-string err))))))

;;;###autoload
(defun create-emacs-package (name)
  "Scaffold a new Emacs package named NAME.
Creates under `create-emacs-package-projects-root' with standard
project structure, GitHub Actions CI, and buttercup test skeleton."
  (interactive "sPackage name: ")
  (let* ((name (string-trim (replace-regexp-in-string "\\.el$" "" name)))
         (_ (let ((case-fold-search nil))
              (unless (string-match-p "\\`[a-z][a-z0-9-]*\\'" name)
                (user-error "Invalid package name: %s (lowercase, alphanumeric, hyphens)" name))))
         (gh-user (string-trim (shell-command-to-string "git config github.user")))
         (root (expand-file-name
                gh-user (or magit-clone-default-directory "~/GitHub/")))
         (pkg-dir (expand-file-name (concat name ".el") root))
         (_ (when (file-exists-p pkg-dir)
              (user-error "Directory already exists: %s" pkg-dir)))
         (author (or user-full-name
                     (string-trim (shell-command-to-string "git config user.name"))))
         (email (or user-mail-address
                    (string-trim (shell-command-to-string "git config user.email"))))
         (year (format-time-string "%Y"))
         (date (format-time-string "%B %-d, %Y"))
         (iso-date (format-time-string "%Y-%m-%d"))
         (replacements `(("{{PKG}}" . ,name)
                         ("{{AUTHOR}}" . ,author)
                         ("{{EMAIL}}" . ,email)
                         ("{{GH_USER}}" . ,gh-user)
                         ("{{YEAR}}" . ,year)
                         ("{{DATE}}" . ,date)
                         ("{{ISO_DATE}}" . ,iso-date))))
    (make-directory (expand-file-name ".github/workflows" pkg-dir) t)
    (make-directory (expand-file-name "test" pkg-dir) t)
    (create-emacs-package--write-template
     "PKG.el.tmpl" (expand-file-name (concat name ".el") pkg-dir) replacements)
    (create-emacs-package--write-template
     "Makefile.tmpl" (expand-file-name "Makefile" pkg-dir) replacements)
    (create-emacs-package--write-template
     "README.org.tmpl" (expand-file-name "README.org" pkg-dir) replacements)
    (create-emacs-package--write-template
     "changelog.org.tmpl" (expand-file-name "changelog.org" pkg-dir) replacements)
    (create-emacs-package--write-template
     "gitignore.tmpl" (expand-file-name ".gitignore" pkg-dir) replacements)
    (create-emacs-package--write-template
     "run-tests.yml.tmpl" (expand-file-name ".github/workflows/run-tests.yml" pkg-dir) replacements)
    (create-emacs-package--write-template
     "PKG-tests.el.tmpl" (expand-file-name (format "test/%s-tests.el" name) pkg-dir) replacements)
    (create-emacs-package--download-license pkg-dir)
    (let ((default-directory pkg-dir))
      (call-process "git" nil nil nil "init"))
    (message "Created package scaffold at %s" pkg-dir)
    pkg-dir))
