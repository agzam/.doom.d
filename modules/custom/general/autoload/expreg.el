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
      (org-mode (insert (org-link-make-string url txt)))
      (markdown--mode (markdown-insert-inline-link txt url))
      (t url))))

;;;###autoload
(transient-define-prefix expreg-transient ()
  "expand/contract"
  [[("v" "expand" expreg-expand :transient t)]
   [("V" "contract" expreg-contract :transient t)]]
  ["Editing"
   :hide always
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      'expreg-transient
      ;; sets up 'special' keys for this transient,
      ;;
      ;; - for the string nominal of the key - calls the command that
      ;;   normally binds to it, exiting the transient
      ;;
      ;; - alternatively, can be a list with the key, transient flag,
      ;; and the command - if you want to explicitly
      ;; override the one that normally binds to the key.
      (thread-last
        '("d" "p" "P" "r" "c" "R" "t" "T" "f" "F" "n" "C-;" "g" "G"
          "SPC" "," ":" "M-x" "M-:" "`" "C-h"
          "s-k" "s-]" "s-j" "s-]"
          ">" "<" "=" "~"  "[" "]"
          ("s" nil evil-surround-region)
          ("j" t evil-next-visual-line)
          ("k" t evil-previous-visual-line)
          ("h" t evil-backward-char)
          ("l" t evil-forward-char)
          ("%" t evilmi-jump-items)
          ("0" t evil-beginning-of-line)
          ("y" t evil-yank)
          ("C-l" t) ("C-e" t)  ("C-y" t)
          ("w" t) ("W" t) ("b" t) ("B" t) ("o" t)  ("$" t)
          ("/" t) ("{" t) ("}" t)
          ("x" nil (lambda () (interactive) (general--simulate-keys nil "SPC x"))))
        (mapcar
         (lambda (key-map)
           (let* ((key (if (stringp key-map) key-map (car key-map)))
                  (explicit-cmd (ignore-errors (nth 2 key-map)))
                  (transient? (and (listp key-map) (cadr key-map)))
                  (cmd (or explicit-cmd
                           (lambda ()
                             (interactive)
                             (if transient?
                                 (call-interactively
                                  (or (lookup-key evil-motion-state-map (kbd key))
                                      (lookup-key evil-visual-state-map (kbd key))
                                      (lookup-key evil-normal-state-map (kbd key))
                                      (lookup-key global-map (kbd key))))
                               (general--simulate-keys nil key)))))
                  (desc (format "%s" key)))
             (list key desc cmd :transient transient?)))))))]
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
    ("; l" "insert link" org-insert-link)
    ("; L" "insert browser url" expreg-transient--insert-browser-url)
    ("; q" "wrap in quote block"
     (lambda () (interactive) (org-wrap-in-block 'quote)))
    ("; s" "wrap in source block"
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
    ("; s" "wrap in code block" markdown-wrap-code-generic)
    ("; <" "wrap in collapsible" markdown-wrap-collapsible)]])
