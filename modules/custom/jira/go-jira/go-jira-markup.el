;;; go-jira-markup.el --- Jira markup to Org-mode converter -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira, markup
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Convert Jira wiki markup to Org-mode format.
;; Based on Jira's wiki markup syntax:
;; https://jira.atlassian.com/secure/WikiRendererHelpAction.jspa?section=all
;;
;; Improvements over existing tools like j2m:
;; - Proper handling of nested lists
;; - Better code block protection
;; - Correct table conversion
;; - Block quote support
;; - Handles edge cases and nested formatting

;;; Code:

(require 'cl-lib)

;;; Protection mechanism for code blocks

(defvar go-jira-markup--protected-blocks nil
  "Alist of (placeholder . original-content) for protected blocks.")

(defun go-jira-markup--protect-block (content)
  "Protect CONTENT from processing by replacing it with a placeholder.
Returns the placeholder string."
  (let ((placeholder (format "<<<PROTECTED-BLOCK-%d>>>" (length go-jira-markup--protected-blocks))))
    (push (cons placeholder content) go-jira-markup--protected-blocks)
    placeholder))

(defun go-jira-markup--restore-blocks (text)
  "Restore all protected blocks in TEXT."
  (dolist (pair go-jira-markup--protected-blocks)
    (setq text (replace-regexp-in-string
                (regexp-quote (car pair))
                (cdr pair)
                text t t)))
  text)

;;; Block-level conversions

(defun go-jira-markup--convert-code-blocks (text)
  "Convert Jira code blocks to Org-mode source blocks and protect them.
Handles {code:lang}...{code} and {noformat}...{noformat}."
  ;; Code blocks with language
  (setq text (replace-regexp-in-string
              "{code:\\([^}]+\\)}\\(\\(?:.\\|\n\\)*?\\){code}"
              (lambda (match)
                (let* ((lang (match-string 1 match))
                       (content (match-string 2 match))
                       (org-block (format "#+begin_src %s\n%s#+end_src" lang content)))
                  (go-jira-markup--protect-block org-block)))
              text))
  
  ;; Code blocks without language
  (setq text (replace-regexp-in-string
              "{code}\\(\\(?:.\\|\n\\)*?\\){code}"
              (lambda (match)
                (let* ((content (match-string 1 match))
                       (org-block (format "#+begin_src\n%s#+end_src" content)))
                  (go-jira-markup--protect-block org-block)))
              text))
  
  ;; Noformat blocks
  (setq text (replace-regexp-in-string
              "{noformat}\\(\\(?:.\\|\n\\)*?\\){noformat}"
              (lambda (match)
                (let* ((content (match-string 1 match))
                       (org-block (format "#+begin_example\n%s#+end_example" content)))
                  (go-jira-markup--protect-block org-block)))
              text))
  
  text)

(defun go-jira-markup--convert-quote-blocks (text)
  "Convert Jira quote blocks to Org-mode quote blocks.
Handles both {quote}...{quote} and bq. syntax."
  ;; Multi-line quotes
  (setq text (replace-regexp-in-string
              "{quote}\\(\\(?:.\\|\n\\)*?\\){quote}"
              "#+begin_quote\n\\1#+end_quote"
              text))
  
  ;; Single-line block quotes
  (setq text (replace-regexp-in-string
              "^bq\\. \\(.+\\)$"
              "#+begin_quote\n\\1\n#+end_quote"
              text))
  
  text)

(defun go-jira-markup--convert-headings (text)
  "Convert Jira headings to Org-mode headings.
h1. → ***, h2. → ****, etc. (offset by 2 since issue is level 2)."
  (setq text (replace-regexp-in-string "^h1\\. \\(.+\\)$" "*** \\1" text))
  (setq text (replace-regexp-in-string "^h2\\. \\(.+\\)$" "**** \\1" text))
  (setq text (replace-regexp-in-string "^h3\\. \\(.+\\)$" "***** \\1" text))
  (setq text (replace-regexp-in-string "^h4\\. \\(.+\\)$" "****** \\1" text))
  (setq text (replace-regexp-in-string "^h5\\. \\(.+\\)$" "******* \\1" text))
  (setq text (replace-regexp-in-string "^h6\\. \\(.+\\)$" "******** \\1" text))
  text)

(defun go-jira-markup--convert-lists (text)
  "Convert Jira lists to Org-mode lists.
Handles both bulleted (*, **, ***) and numbered (#, ##, ###) lists."
  (let ((lines (split-string text "\n"))
        (result '()))
    (dolist (line lines)
      (cond
       ;; Numbered list: # → 1., ## → "  1.", ### → "    1."
       ((string-match "^\\(#+\\) \\(.+\\)$" line)
        (let* ((level (match-string 1 line))
               (content (match-string 2 line))
               (indent (make-string (* (1- (length level)) 2) ?\s)))
          (push (format "%s1. %s" indent content) result)))
       
       ;; Bulleted list: * → -, ** → "  -", *** → "    -"
       ((string-match "^\\([*+-]+\\) \\(.+\\)$" line)
        (let* ((level (match-string 1 line))
               (content (match-string 2 line))
               (indent (make-string (* (1- (length level)) 2) ?\s)))
          (push (format "%s- %s" indent content) result)))
       
       ;; Not a list item
       (t (push line result))))
    
    (mapconcat #'identity (nreverse result) "\n")))

(defun go-jira-markup--convert-tables (text)
  "Convert Jira tables to Org-mode tables.
Handles || for headers and | for regular cells."
  (let ((lines (split-string text "\n"))
        (result '())
        (in-table nil))
    (dolist (line lines)
      (cond
       ;; Header row: ||cell1||cell2|| → |cell1|cell2|
       ((string-match "||" line)
        (let ((org-line (replace-regexp-in-string "||" "|" line)))
          (push org-line result)
          ;; Add separator after header
          (when (not in-table)
            (let* ((cells (split-string org-line "|" t))
                   (separator (concat "|" (mapconcat (lambda (_) "---") cells "|") "|")))
              (push separator result)
              (setq in-table t)))))
       
       ;; Regular row with pipes
       ((and (string-match "^|" line) (string-match "|$" line))
        (push line result))
       
       ;; End of table
       (t
        (setq in-table nil)
        (push line result))))
    
    (mapconcat #'identity (nreverse result) "\n")))

;;; Inline conversions

(defun go-jira-markup--convert-inline-formatting (text)
  "Convert Jira inline formatting to Org-mode.
Handles bold, italic, monospace, etc."
  ;; Protect inline code first (monospace)
  (setq text (replace-regexp-in-string
              "{{\\([^}]+\\)}}"
              (lambda (match)
                (go-jira-markup--protect-block (format "~%s~" (match-string 1 match))))
              text))
  
  ;; Bold: *text* → **text**
  (setq text (replace-regexp-in-string
              "\\(^\\|[[:space:]]\\)\\*\\([^*\n]+?\\)\\*\\([[:space:]]\\|$\\)"
              "\\1**\\2**\\3"
              text))
  
  ;; Italic: _text_ → /text/
  (setq text (replace-regexp-in-string
              "\\(^\\|[[:space:]]\\)_\\([^_\n]+?\\)_\\([[:space:]]\\|$\\)"
              "\\1/\\2/\\3"
              text))
  
  ;; Strikethrough: -text- → +text+
  (setq text (replace-regexp-in-string
              "\\(^\\|[[:space:]]\\)-\\([^-\n]+?\\)-\\([[:space:]]\\|$\\)"
              "\\1+\\2+\\3"
              text))
  
  ;; Insert: +text+ → _text_
  (setq text (replace-regexp-in-string
              "\\(^\\|[[:space:]]\\)\\+\\([^+\n]+?\\)\\+\\([[:space:]]\\|$\\)"
              "\\1_\\2_\\3"
              text))
  
  ;; Superscript: ^text^ → ^{text}
  (setq text (replace-regexp-in-string
              "\\^\\([^^]+?\\)\\^"
              "^{\\1}"
              text))
  
  ;; Subscript: ~text~ → _{text}
  (setq text (replace-regexp-in-string
              "~\\([^~]+?\\)~"
              "_{\\1}"
              text))
  
  ;; Citation: ??text?? → /text/ (escape the question marks!)
  (setq text (replace-regexp-in-string
              "\\?\\?\\([^?]+?\\)\\?\\?"
              "/\\1/"
              text))
  
  text)

(defun go-jira-markup--convert-links (text)
  "Convert Jira links to Org-mode links.
Handles [text|url], [url], and bare URLs."
  ;; Links with text: [text|url] → [[url][text]]
  ;; Capture preceding space/punctuation to preserve it
  (setq text (replace-regexp-in-string
              "\\([[:space:](]\\|^\\)\\[\\([^]|]+\\)|\\([^]]+\\)\\]"
              "\\1[[\\3][\\2]]"
              text))
  
  ;; Links without text: [url] → [[url]]
  (setq text (replace-regexp-in-string
              "\\([[:space:](]\\|^\\)\\[\\([^]|]+\\)\\]"
              "\\1[[\\2]]"
              text))
  
  text)

(defun go-jira-markup--convert-images (text)
  "Convert Jira images to Org-mode image links.
Handles !image.png!, !image.png|alt=text!, etc."
  ;; Images with alt text: !image.png|alt=desc! → [[file:image.png][desc]]
  (setq text (replace-regexp-in-string
              "!\\([^|!]+\\)|[^!]*alt=\\([^,!]+\\)[^!]*!"
              "[[file:\\1][\\2]]"
              text))
  
  ;; Images with other params (ignore params): !image.png|params! → [[file:image.png]]
  (setq text (replace-regexp-in-string
              "!\\([^|!]+\\)|[^!]+!"
              "[[file:\\1]]"
              text))
  
  ;; Simple images: !image.png! → [[file:image.png]]
  (setq text (replace-regexp-in-string
              "!\\([^!]+\\)!"
              "[[file:\\1]]"
              text))
  
  text)

(defun go-jira-markup--convert-colors (text)
  "Convert Jira color markup.
{color:red}text{color} → text (strip colors, or wrap in export block)."
  ;; For now, just strip color markup
  (setq text (replace-regexp-in-string
              "{color:[^}]+}\\(\\(?:.\\|\n\\)*?\\){color}"
              "\\1"
              text))
  text)

;;; Main conversion function

;;;###autoload
(defun go-jira-markup-to-org (jira-text)
  "Convert JIRA-TEXT (Jira wiki markup) to Org-mode format.
Returns the converted text as a string."
  (when (and jira-text (not (string-empty-p jira-text)))
    (setq go-jira-markup--protected-blocks nil)
    (let ((text jira-text))
      ;; Phase 1: Protect and convert code blocks
      (setq text (go-jira-markup--convert-code-blocks text))
      
      ;; Phase 2: Block-level conversions
      (setq text (go-jira-markup--convert-quote-blocks text))
      (setq text (go-jira-markup--convert-headings text))
      (setq text (go-jira-markup--convert-lists text))
      (setq text (go-jira-markup--convert-tables text))
      
      ;; Phase 3: Inline conversions
      (setq text (go-jira-markup--convert-inline-formatting text))
      (setq text (go-jira-markup--convert-links text))
      (setq text (go-jira-markup--convert-images text))
      (setq text (go-jira-markup--convert-colors text))
      
      ;; Phase 4: Restore protected blocks
      (setq text (go-jira-markup--restore-blocks text))
      
      text)))

(provide 'go-jira-markup)
;;; go-jira-markup.el ends here
