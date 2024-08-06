;;; custom/tree-sitter/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun tree-sitter-init+ ()
  "Install Tree-sitter grammars if they are absent."
  (interactive)
  (dolist (grammar
           '((css . ("https://github.com/tree-sitter/tree-sitter-css" "master"))
             (html . ("https://github.com/tree-sitter/tree-sitter-html" "master"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json" "master"))
             (python . ("https://github.com/tree-sitter/tree-sitter-python" "master"))
             (toml "https://github.com/tree-sitter/tree-sitter-toml")
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
             (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "master"))
             (clojure . ("https://github.com/sogaiu/tree-sitter-clojure" "master"))
             (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "main"))))
    (add-to-list 'treesit-language-source-alist grammar)
    ;; Only install `grammar' if we don't already have it
    ;; installed. However, if you want to *update* a grammar then
    ;; this obviously prevents that from happening.
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))
