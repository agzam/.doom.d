;;; custom/search/autoload.el -*- lexical-binding: t; -*-

(defcustom engine-mode/github-mode->lang
  '(("clojurescript" . "Clojure")
    ("clojure" . "Clojure")
    ("clojurec" . "Clojure")
    ("emacs-lisp" . "Emacs Lisp")
    ("Info" . "Emacs Lisp")
    ("lisp-data" . "Emacs Lisp")
    ("helpful" . "Emacs Lisp")
    ("js" . "JavaScript"))
  "Associates current mode with a language in Github terms"
  :type 'alist
  :group 'engine)

;;;###autoload (autoload 'engine/search-github-with-lang "custom/search-engine/autoload" nil t)
(defun engine/search-github-with-lang ()
  "Search on Github with attempt of detecting language associated with current-buffer's mode"
  (interactive)
  (let* ((mode-name (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
         (lang (cdr (assoc mode-name engine-mode/github-mode->lang)))
         (lang-term (if lang (concat "language:\"" lang "\" ") ""))
         (current-word (if (region-active-p)
                           (buffer-substring (region-beginning) (region-end))
                         (thing-at-point 'symbol)))
         (search-term (read-string "Search Github: " (concat lang-term current-word))))
    (engine/search-github search-term)))


;;;###autoload
(defun +fasd-find (search-type &optional query)
  "Use fasd to open a file, or a directory with dired.
SEARCH-TYPE can be files, dirs, or both"
  (interactive "P")
  (if (not (executable-find "fasd"))
      (error "Fasd executable cannot be found.  It is required by `fasd.el'.  Cannot use `fasd-find-file'")
    ;; (unless query (setq query (read-from-minibuffer "Fasd query: ")))


    (let* ((fasd-items (split-string
                        (shell-command-to-string
                         (format "fasd -lR%s"
                                 (pcase search-type
                                   ('files "f")
                                   ('dirs "d")
                                   ('both "a"))))))

           (path (completing-read "Choose: " fasd-items)))
      (find-file path))))

;;;###autoload
(defun +add-to-fasd-cache ()
  (let ((file (if (string= major-mode "dired-mode")
                  dired-directory
                (buffer-file-name))))
    (when (and file
               (stringp file)
               (file-readable-p file))
      (start-process "*fasd*" nil "fasd" "--add" file))))
