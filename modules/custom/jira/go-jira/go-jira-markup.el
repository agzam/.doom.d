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
                       (org-block (format "#+begin_src %s\n%s\n#+end_src" lang content)))
                  (go-jira-markup--protect-block org-block)))
              text))
  
  ;; Code blocks without language
  (setq text (replace-regexp-in-string
              "{code}\\(\\(?:.\\|\n\\)*?\\){code}"
              (lambda (match)
                (let* ((content (match-string 1 match))
                       (org-block (format "#+begin_src\n%s\n#+end_src" content)))
                  (go-jira-markup--protect-block org-block)))
              text))
  
  ;; Noformat blocks
  (setq text (replace-regexp-in-string
              "{noformat}\\(\\(?:.\\|\n\\)*?\\){noformat}"
              (lambda (match)
                (let* ((content (match-string 1 match))
                       (org-block (format "#+begin_example\n%s\n#+end_example" content)))
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
Handles numbered (#), bulleted (*), mixed (#*, *#) lists."
  (let ((lines (split-string text "\n"))
        (result '())
        (counters (make-hash-table :test 'equal))
        (last-was-nested nil)) ; Track if last line was #* or *#
    (dolist (line lines)
      (cond
       ;; Mixed: #* → "  -" (bullet under number), *# → "  1." (number under bullet)
       ((string-match "^\\([#*+-]+\\)\\([#*+-]+\\) \\(.+\\)$" line)
        (let* ((prefix1 (match-string 1 line))
               (prefix2 (match-string 2 line))
               (content (match-string 3 line)))
          (cond
           ;; #* pattern: bullet nested under number
           ((and (string-match-p "^#+$" prefix1) (string-match-p "^[*+-]+$" prefix2))
            (let* ((indent (make-string (+ (* (length prefix1) 2)
                                          (* (1- (length prefix2)) 2))
                                       ?\s)))
              (setq last-was-nested 'hash-star)
              (push (format "%s- %s" indent content) result)))
           
           ;; *# pattern: numbered item nested under bullet
           ((and (string-match-p "^[*+-]+$" prefix1) (string-match-p "^#+$" prefix2))
            (let* ((base-indent (* (length prefix1) 2))
                   (num-indent (+ base-indent (* (1- (length prefix2)) 2)))
                   (indent (make-string num-indent ?\s))
                   (level-key (format "%s-%s" prefix1 prefix2))
                   (counter (1+ (gethash level-key counters 0))))
              (puthash level-key counter counters)
              (setq last-was-nested 'star-hash)
              (push (format "%s%d. %s" indent counter content) result)))
           
           ;; Other patterns: just output as-is
           (t (push line result)))))
       
       ;; Numbered list: # → 1., ## → "  1."
       ((string-match "^\\(#+\\) \\(.+\\)$" line)
        (let* ((level (match-string 1 line))
               (content (match-string 2 line)))
          (if (eq last-was-nested 'hash-star)
              ;; After #* items, # text should be plain text (strip #)
              (progn
                (setq last-was-nested nil)
                (push content result))
            ;; Normal numbered list
            (let* ((indent (make-string (* (1- (length level)) 2) ?\s))
                   (level-key (length level))
                   (counter (1+ (gethash level-key counters 0))))
              (puthash level-key counter counters)
              (maphash (lambda (k v)
                         (when (> k level-key)
                           (puthash k 0 counters)))
                       counters)
              (setq last-was-nested nil)
              (push (format "%s%d. %s" indent counter content) result)))))
       
       ;; Bulleted list: * → -, ** → "  -"
       ((string-match "^\\([*+-]+\\) \\(.+\\)$" line)
        (let* ((level (match-string 1 line))
               (content (match-string 2 line))
               (indent (make-string (* (1- (length level)) 2) ?\s)))
          (setq last-was-nested nil)
          (push (format "%s- %s" indent content) result)))
       
       ;; Empty line or non-list content - reset
       (t 
        (when (string-empty-p (string-trim line))
          (clrhash counters)
          (setq last-was-nested nil))
        (push line result))))
    
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
  ;; Protect inline code first (monospace): {{...}}
  ;; Handle backslashes: \\ in Jira = \ in output
  (setq text (replace-regexp-in-string
              "{{\\([^}]+\\)}}"
              (lambda (match)
                (let* ((content (match-string 1 match))
                       ;; Convert Jira \\ to org \\
                       (content (replace-regexp-in-string "\\\\\\\\" "\\\\\\\\" content)))
                  ;; Add hair space around code to prevent org-mode interpretation issues
                  (go-jira-markup--protect-block (format "\u200A~%s~\u200A" content))))
              text))
  
  ;; Bold: *text* → *text* (org-mode uses single asterisk for bold)
  ;; No conversion needed, already correct
  
  ;; Italic: _text_ → /text/
  (setq text (replace-regexp-in-string
              "\\(^\\|[[:space:]]\\)_\\([^_\n]+?\\)_\\([[:space:]]\\|$\\)"
              "\\1/\\2/\\3"
              text))
  
  ;; Strikethrough: -text- → +text+
  ;; Only match if preceded by space/tab (not newline, which would catch list bullets)
  (setq text (replace-regexp-in-string
              "\\([ \t]\\)-\\([^-\n]+?\\)-\\([ \t]\\|$\\)"
              "\\1+\\2+\\3"
              text))
  
  ;; Insert: +text+ → _text_
  (setq text (replace-regexp-in-string
              "\\(^\\|[ \t]\\)\\+\\([^+\n]+?\\)\\+\\([ \t]\\|$\\)"
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
Handles [text|url|smart-link], [text|url], and [url]."
  ;; Links with 3 parts (smart-link): [text|url|smart-link] → [[url][text]]
  (setq text (replace-regexp-in-string
              "\\[\\([^]|]+\\)|\\([^]|]+\\)|[^]]+\\]"
              "[[\\2][\\1]]"
              text))
  
  ;; Links with 2 parts: [text|url] → [[url][text]]
  (setq text (replace-regexp-in-string
              "\\[\\([^]|]+\\)|\\([^]|]+\\)\\]"
              "[[\\2][\\1]]"
              text))
  
  ;; Links with 1 part: [url] → [[url]]
  ;; But don't match if already converted to org format [[...]]
  (setq text (replace-regexp-in-string
              "\\(^\\|[^[]\\)\\[\\([^]|[]+\\)\\]\\($\\|[^]]\\)"
              "\\1[[\\2]]\\3"
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

(defun go-jira-markup--convert-datetimes (text)
  "Convert ISO 8601 datetimes to Org-mode inactive timestamps.
e.g., 2025-12-04T22:20:04.549+0100 → [2025-12-04 Thu 22:20]"
  (replace-regexp-in-string
   "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):[0-9.]\\{2,\\}[+-][0-9]\\{4\\}"
   (lambda (match)
     (let* ((year (match-string 1 match))
            (month (match-string 2 match))
            (day (match-string 3 match))
            (hour (match-string 4 match))
            (minute (match-string 5 match))
            (time-str (format "%s-%s-%s %s:%s" year month day hour minute))
            (time (date-to-time time-str))
            (dow (format-time-string "%a" time)))
       (format "[%s-%s-%s %s %s:%s]" year month day dow hour minute)))
   text))

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
      (setq text (go-jira-markup--convert-datetimes text))
      
      ;; Phase 4: Restore protected blocks
      (setq text (go-jira-markup--restore-blocks text))
      
      ;; Phase 5: Clean up whitespace
      (setq text (go-jira-markup--clean-whitespace text))
      
      text)))

(defun go-jira-markup--clean-whitespace (text)
  "Clean up excessive whitespace in TEXT.
- Collapse multiple empty lines into one
- Remove empty lines immediately after headings"
  ;; First collapse multiple empty lines into one
  (setq text (replace-regexp-in-string "\n\n\n+" "\n\n" text))
  
  ;; Remove empty lines immediately after org headings
  ;; Pattern: heading line (with newline) followed by another newline (empty line)
  (setq text (replace-regexp-in-string "\\(^\\*+ .+\n\\)\n" "\\1" text))
  
  text)

(provide 'go-jira-markup)
;;; go-jira-markup.el ends here
