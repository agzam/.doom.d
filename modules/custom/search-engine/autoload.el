;;; custom/search-engine/autoload.el -*- lexical-binding: t; -*-

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
