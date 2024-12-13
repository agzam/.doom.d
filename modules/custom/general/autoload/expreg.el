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
(transient-define-prefix expreg-transient ()
  "expand/contract"
  [[("v" "expand" expreg-expand :transient t)]
   [("V" "contract" expreg-contract :transient t)]]
  ["Editing"
   :hide always
   [("RET" "ignore" transient-quit-one)
    ("y" "y" evil-yank)
    ("d" "d" evil-delete)
    ("x" "x" evil-delete-char)
    ("p" "p" evil-paste-after)
    ("P" "P" evil-paste-before)
    ("r" "r" evil-replace)
    ("c" "c" evil-change)
    ("s" "s" evil-surround-region)
    ("R" "R" evil-multiedit-match-all)
    ("o" "o" exchange-point-and-mark :transient t)
    ("0" "0" evil-beginning-of-line :transient t)
    ("$" "$" evil-end-of-line :transient t)
    ("k" "k" evil-previous-visual-line :transient t)
    ("j" "j" evil-next-visual-line :transient t)
    ("h" "h" evil-backward-char :transient t)
    ("l" "l" evil-forward-char :transient t)
    ("%" "%" evilmi-jump-items :transient t)
    ("C-;" "embark-action" embark-act)
    (">" "indent" indent-rigidly)
    ("~" "invert" evil-invert-char)
    ("SPC" "space" (lambda () (interactive) (funcall (general-simulate-key "SPC"))))]]
  ["Org Mode"
   :if (lambda () (derived-mode-p 'org-mode))
   :hide (lambda () (not transient-show-common-commands))
   [("*" "bold" (lambda () (interactive) (org-emphasize ?*)))
    ("/" "italic" (lambda () (interactive) (org-emphasize ?\/)))
    ("_" "underline" (lambda () (interactive) (org-emphasize ?_)))
    ("=" "verbatim" (lambda () (interactive) (org-emphasize ?=)))
    ("`" "code" (lambda () (interactive) (org-emphasize ?~)))
    ("+" "strikethrough" (lambda () (interactive) (org-emphasize ?+)))]
   [("C-c l" "insert link" org-insert-link)
    ("C-c L" "insert browser url"
     (lambda ()
       (interactive)
       (when-let* ((url (browser-copy-tab-link))
                   (_ (string-match-p "^https?://"  url))
                   (rb (region-beginning))
                   (re (region-end))
                   (txt (buffer-substring-no-properties rb re))
                   (lnk (org-link-make-string url txt)))
         (delete-region rb re)
         (insert (org-link-make-string url txt)))))
    ("C-c t" "wrap in quote block"
     (lambda () (interactive) (org-wrap-in-block 'quote)))
    ("C-c s" "wrap in source block"
     (lambda () (interactive) (org-wrap-in-block 'src)))]]
  ["Markdown"
   :if (lambda () (derived-mode-p 'markdown-mode))
   :hide (lambda () (not transient-show-common-commands))
   [("*" "bold" markdown-insert-bold)
    ("/" "italic" markdown-insert-italic)
    ("`" "code" markdown-insert-code)
    ("+" "strikethrough" markdown-insert-strike-through)]
   [("C-c l" "insert link" markdown-insert-link)
    ("C-c L" "insert browser url"
     (lambda ()
       (interactive)
       (when-let* ((url (browser-copy-tab-link))
                   (_ (string-match-p "^https?://"  url))
                   (rb (region-beginning))
                   (re (region-end))
                   (txt (buffer-substring-no-properties rb re)))
         (delete-region rb re)
         (markdown-insert-inline-link txt url))))
    ("C-c s" "wrap in code block" markdown-wrap-code-generic)]])
