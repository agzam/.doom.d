;;; custom/search/autoload/search.el -*- lexical-binding: t; -*-

(defvar +search--github-mode->lang
  '(((clojurescript clojure cider-clojure-interaction) . "Clojure")
    ((emacs-lisp Info lisp-data helpful) . "Emacs Lisp")
    ((tsx-ts) . "TypeScript")
    ((js) . "JavaScript")
    ((fennel) . "Fennel"))
  "Associates current mode with a language in Github terms")

;;;###autoload
(defun +search-github-with-lang ()
  "Search on Github with attempt of detecting language associated with current-buffer's mode"
  (interactive)
  (let* ((mode (intern (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
         (lang (cl-some (lambda (entry)
                          (when (memq mode (car entry))
                            (cdr entry)))
                        +search--github-mode->lang))
         (lang-term (if lang (concat "language:\"" lang "\" ") ""))
         (word-at-point (if (region-active-p)
                            (buffer-substring (region-beginning) (region-end))
                          (thing-at-point 'symbol)))
         (search-term (read-string "Search Github: " (concat lang-term word-at-point)))
         (query (format "https://github.com/search?q=%s&type=code"
                        (url-hexify-string search-term))))
    (browse-url query)))

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
