;;; custom/search/autoload/search.el -*- lexical-binding: t; -*-

(defvar +search--github-mode->lang
  '(((clojurescript clojure cider-clojure-interaction) . "Clojure")
    ((emacs-lisp Info lisp-data helpful) . "Emacs Lisp")
    ((tsx-ts) . "TypeScript")
    ((js jtsx-jsx) . "JavaScript")
    ((fennel) . "Fennel"))
  "Associates current mode with a language in Github terms")

;;;###autoload
(defun +search-github-with-lang ()
  "Search on Github with attempt of detecting language associated with current-buffer's mode"
  (interactive)
  (let* ((lang (cl-some (lambda (entry)
                          (when (cl-intersection
                                 (seq-map #'symbol-name
                                          (derived-mode-all-parents major-mode))
                                 (seq-map (lambda (x) (concat (symbol-name x) "-mode"))
                                          (car entry))
                                 :test #'equal)
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
    (let* ((items (thread-last
                    (or query "")
                    (format "zoxide query --list '%s'")
                    shell-command-to-string
                    ((lambda (s) (split-string s "\n")))
                    (seq-remove #'string-blank-p)))
           (path (or (and (length= items 1) (car-safe items))
                     (consult--read
                      items
                      :prompt "Choose: "
                      :sort nil
                      :initial query))))
      (if (eq major-mode 'eshell-mode)
          path
        (find-file path)))))

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

;; +default/search-project broke in Doom due to
;; https://github.com/minad/consult/commit/ada079d5932700a8819ace622ef4323e73983161
;;;###autoload
(defun search-in-project ()
  (interactive)
  (consult-ripgrep
   (project-root (project-current))
   (if (use-region-p)
       (buffer-substring-no-properties
        (region-beginning) (region-end))
     (symbol-name (symbol-at-point)))))
