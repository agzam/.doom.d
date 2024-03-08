;;; custom/search/autoload.el -*- lexical-binding: t; -*-

(defcustom engine-mode/github-mode->lang
  '(("clojurescript" . "Clojure")
    ("clojure" . "Clojure")
    ("clojurec" . "Clojure")
    ("emacs-lisp" . "Emacs Lisp")
    ("Info" . "Emacs Lisp")
    ("lisp-data" . "Emacs Lisp")
    ("helpful" . "Emacs Lisp")
    ("js" . "JavaScript")
    ("fennel" . "Fennel"))
  "Associates current mode with a language in Github terms"
  :type 'alist
  :group 'engine)

;;;###autoload
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
(defun +zoxide-find (&optional query)
  "Use zoxide to open a directory with dired."
  (interactive "P")
  (if (not (executable-find "zoxide"))
      (error "zoxide executable cannot be found")
    (let* ((items (split-string
                   (shell-command-to-string "zoxide query --list")
                   "\n"))
           (path (completing-read "Choose: " items)))
      (find-file path))))

;;;###autoload
(defun +add-to-zoxide-cache ()
  (let ((file (if (string= major-mode "dired-mode")
                  dired-directory
                (file-name-directory (buffer-file-name)))))
    (when (and file
               (stringp file)
               (file-readable-p file))
      (call-process-shell-command
       (format "zoxide add \"%s\"" file)))))
