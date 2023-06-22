;;; custom/chat/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +decrypt-open-ai-token ()
  "Retrieves encrypted open-ai token from auth-sources."
  (interactive)
  (if (eq gptel-api-key '+decrypt-open-ai-token)
      (setq gptel-api-key
            (funcall
             (plist-get
              (car
               (auth-source-search :host "api.openai.com" :login "gptel" :type 'netrc :max 1))
              :secret)))
    gptel-api-key))

(defun +replace-region-with-string (replacement)
  "Replace region or buffer content with REPLACEMENT."
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point-min) (point-max)))
  (insert replacement))

(defvar chatgpt-improve-text-hist
  '("Improve this text, don't be too formal:"
    "Improve this code:"
    "Add comments to the following code snippet:"
    "Improve and make it witty:"
    "Improve and add some humor:"))

;;;###autoload
(defun +chatgpt-shell-improve-text (prompt-str)
  "Send given text to chat-gpt for given PROMPT-STR."
  (interactive "P")
  (message "beep-bop... checking your crap...")
  (let* ((text (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning)
                    (region-end))
                 (buffer-substring-no-properties
                  (point-min)
                  (point-max))))
         (default-prompt "Improve the following text:")
         (prompt (if prompt-str
                     (read-string "Prompt to use: "
                                  default-prompt
                                  'chatgpt-improve-text-hist)
                   default-prompt))
         (new-text (chatgpt-shell-post-prompt
                    (format "%s\n%s" prompt text)))
         (fst-buf (with-current-buffer (generate-new-buffer " * chat-gpt text 1 *")
                    (insert text)
                    (current-buffer)))
         (snd-buf (with-current-buffer (generate-new-buffer " * chat-gpt text 2 *")
                    (insert new-text)
                    (current-buffer)))
         (diff-win (diff fst-buf snd-buf "--text" 'no-async)))
    (+replace-region-with-string new-text)
    (message "I hope you like it")

    ;; cleaner diff
    (with-current-buffer (window-buffer diff-win)
      (read-only-mode -1)
      (goto-char (point-min))
      (dolist (r '("^diff.*\n"
                   "^. No newline at end of file\n"
                   "^. No newline at end of file\n"
                   "^Diff finished.*$"))
        (re-search-forward r nil :noerror)
        (replace-match ""))
      (visual-line-mode))
    (kill-buffer fst-buf)
    (kill-buffer snd-buf)))


(defun +reduce-buffer-content-to (max-words)
  "Trim current buffer content to contain no more than `MAX-WORDS'."
  (with-current-buffer (current-buffer)
    (let* ((words (split-string (buffer-string) "\\b"))
           (count 0)
           (str-word-p (lambda (s) (string-match-p "^[[:alnum:]]+$" s)))
           (content (seq-reduce
                     (lambda (acc x)
                       (when (funcall str-word-p x)
                         (cl-incf count))
                       (if (<= count max-words)
                           (concat acc x)
                         acc))
                     words "")))
      (erase-buffer)
      (insert content))))

(ert-deftest test-reduce-buffer-to-words ()
  "Test that the function reduces the buffer to the correct number of words."
  (let ((max-words 9))
    (should
     (equal
      max-words
      (with-temp-buffer
        (insert "This is a test. This is only a test. Please follow the instructions carefully.")
        (+reduce-buffer-content-to max-words)
        (count-words (point-min) (point-max)))))))

;;;###autoload
(defun +retrive-text-content-from-page (url)
  "Return document.body.innerText for given html page at URL."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (let* ((_ (xml-remove-comments (point-min) (point-max)))
           (parsed (libxml-parse-html-region (point-min) (point-max))))
      (thread-last
        (dom-child-by-tag parsed 'body)
        (seq-remove (lambda (x)
                      (or
                       ;; remove non-text tags
                       (member (car-safe x) '(meta comment link script style))
                       (and (listp x)
                            ;; remove HTTP:1.1 200 OK stuff
                            (string-match-p "^HTTP/[[:digit:]].[[:digit:]] [[:digit:]]+"
                                            (dom-text x))))))
        (dom-texts)
        (replace-regexp-in-string "\\s-+" " ")))))

;;;###autoload
(defun +chat-gpt-page-summary (ref title)
  (require 'deferred)
  (require 'request-deferred)
  (let* ((content
          (thread-first
            ref
            +retrive-text-content-from-page
            split-string
            (seq-take 960) ; try not to exceed token limit, which is ~1000 words
            (string-join " ")))
         (prompt-template
          (format
           (mapconcat
            #'identity
            '("Summarize info from the page: %s"
              "Based on URL and extrapolate from provided content:"
              "--begin-content--\n%s\n--end-content--"
              "For new terms and phrases add Wikipedia links."
              "Optionally, add books (with ISBNs) and related academic papers (with URLs)."
              "Output results using the following Org-Mode template:" ""
              "* Summary"
              "{{text-summary}}" ""
              "* Papers & Books"
              "- {{link-1}}" "- {{link-2}}" "- {{link-n}}" ""
              "* Wikipedia"
              "- {{wiki-link-1}}" "- {{wiki-link-2}}" "- {{wiki-link-n}}" "")
            "\n")
           ref content))
         (_ (message "beep-bop... analyzing the crap from %s" ref))
         ;; (chatgpt-shell-model-temperature 1.5)
         (summary-string (chatgpt-shell-post-prompt prompt-template))
         (links-content
          (with-current-buffer (+find-related-pages-serpapi ref)
            (let ((content (buffer-string)))
              (kill-buffer)
              content))))
    (with-temp-buffer
      (insert (format "#+title: %s\n\n" title))
      (insert summary-string)
      (insert "\n\n")
      (insert "* Other Links\n")
      (insert links-content)
      (buffer-string))))
