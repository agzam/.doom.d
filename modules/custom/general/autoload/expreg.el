;;; custom/general/autoload/expreg.el -*- lexical-binding: t; -*-

;;;###autoload
(defun expreg--line ()
  "Return a list of regions containing surrounding sentences."
  (ignore-errors
    (let (beg end)
      (end-of-visual-line)
      (setq end (point))
      (beginning-of-visual-line)
      (setq beg (point))
      `((line . ,(cons beg end))))))

;;;###autoload
(defun expreg-transient--insert-browser-url ()
  (interactive)
  (when-let* ((url (browser-copy-tab-link))
              (_ (string-match-p "^https?://"  url))
              (rb (region-beginning))
              (re (region-end))
              (txt (buffer-substring-no-properties rb re)))
    (delete-region rb re)
    (pcase major-mode
      ('org-mode (insert (org-link-make-string url txt)))
      ('markdown-mode (markdown-insert-inline-link txt url))
      (_ url))))

;;;###autoload
(transient-define-prefix expreg-transient ()
  "expand/contract"
  [[("v" "expand" expreg-expand :transient t)]
   [("V" "contract" expreg-contract :transient t)]]
  ["bypass keys"
   :hide always
   :setup-children
   (lambda (_)
     (transient-bypass-keys
      'expreg-transient
      '("d" "p" "P" "r" "c" "R" "t" "T" "f" "F" "n" "C-;"
        "SPC" "," ":" "M-x" "M-:" "`" "C-h" "C-x TAB"
        "s-k" "s-]" "s-j" "s-]"
        ">" "<" "=" "~"  "[" "]" "J" "s"
        ("*" nil evil-ex-search-word-forward)
        ("#" nil evil-ex-search-word-backward)
        ("j" t evil-next-visual-line)
        ("k" t evil-previous-visual-line)
        ("h" t evil-backward-char)
        ("l" t evil-forward-char)
        ("%" t evilmi-jump-items)
        ("0" t evil-beginning-of-line)
        ("y" t evil-yank)
        ("o" t exchange-point-and-mark)
        ("C-l" t) ("C-e" t)  ("C-y" t)
        ("w" t) ("W" t) ("b" t) ("B" t)  ("$" t)
        ("/" t) ("{" t) ("}" t)
        ("g" t evil-goto-first-line) ("G" t evil-goto-line)
        ("x" nil (lambda () (interactive) (general--simulate-keys nil "SPC x"))))))]
  ["Misc"
   :hide (lambda () (not transient-show-common-commands))
   [("u" (lambda () (interactive) (undo) (evil-visual-restore)) :transient t)]
   [("C-r" (lambda () (interactive) (undo-redo) (evil-visual-restore)) :transient t)]]
  ["Org Mode"
   :if (lambda () (derived-mode-p 'org-mode))
   :hide (lambda () (not transient-show-common-commands))
   [("; *" "bold" (lambda () (interactive) (org-emphasize ?*)))
    ("; /" "italic" (lambda () (interactive) (org-emphasize ?\/)))
    ("; _" "underline" (lambda () (interactive) (org-emphasize ?_)))
    ("; =" "verbatim" (lambda () (interactive) (org-emphasize ?=)))
    ("; `" "code" (lambda () (interactive) (org-emphasize ?~)))
    ("; +" "strikethrough" (lambda () (interactive) (org-emphasize ?+)))]
   [("C-c l" "insert link" org-insert-link)
    ("C-c L" "insert browser url" expreg-transient--insert-browser-url)
    ("C-c i" "insert org-roam link" vulpea-insert)
    ("; l" "insert link" org-insert-link)
    ("; L" "insert browser url" expreg-transient--insert-browser-url)
    ("; q" "wrap in quote block"
     (lambda () (interactive) (org-wrap-in-block 'quote)))
    ("; c" "wrap in source block"
     (lambda () (interactive) (org-wrap-in-block 'src)))]]
  ["Markdown"
   :if (lambda () (derived-mode-p 'markdown-mode))
   :hide (lambda () (not transient-show-common-commands))
   [("; *" "bold" markdown-insert-bold)
    ("; /" "italic" markdown-insert-italic)
    ("; `" "code" markdown-insert-code)
    ("; +" "strikethrough" markdown-insert-strike-through)]
   [("C-c l" "insert link" markdown-insert-link)
    ("C-c L" "insert browser url" expreg-transient--insert-browser-url)
    ("; l" "insert link" markdown-insert-link)
    ("; L" "insert browser url" expreg-transient--insert-browser-url)
    ("; c" "wrap in code block" markdown-wrap-code-generic)
    ("; <" "wrap in collapsible" markdown-wrap-collapsible)]]
  ["Clojure"
   :if (lambda () (derived-mode-p 'clojure-mode))
   :hide (lambda () (not transient-show-common-commands))
   [("; c" "wrap comment" clojure-wrap-rich-comment)]]
  ["Magit"
   :if (lambda () (derived-mode-p 'magit-mode))
   :hide always
   [("s" "stage" (lambda () (interactive) (general--simulate-keys nil "s")))
    ("u" "unstage" (lambda () (interactive) (general--simulate-keys nil "u")))
    ("x" "discard" (lambda () (interactive) (general--simulate-keys nil "x")))
    ("a" "apply" (lambda () (interactive) (general--simulate-keys nil "a")))]])
