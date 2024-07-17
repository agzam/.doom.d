;;; custom/ai/gptel.el -*- lexical-binding: t; -*-
(defvar +gptel-improve-text-prompt nil)

(defvar +gptel-improve-text-prompts-history
  (list
   (concat "You are a spelling corrector and text improver. "
           "Only correct mistakes, do not alter the text structure unless stylistic, "
           "orthographic, morphologic and other linguistic errors found. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing if there were no changes.")

   (concat "You are a fact-checker and text enhancer. "
           "Fix mistakes and flag factual inaccuracies, do not alter the text structure "
           "unless it is absolutely necessary. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing if there were no changes.")

   (concat "You are spelling corrector and text enhancer. "
           "Provide 3 different improved variations of the given text, "
           "separating each variant with: "
           "\n\n---\n\n"
           "Do not include any explanations, titles, headers or bullet points "
           "- ONLY plain text of variants, nothing else!")

   (concat "You are an experienced software developer. Explain the given code snippet, "
           "diving into technical details for better understanding. "
           "Suggest a better approach if necessary. "
           "Strive for concise code that is easy-to-reason about. "
           "Optionally, recommend libraries, tools and literature for better "
           "understanding the problem and improving upon it.")

   (concat "You are a great software developer. "
           "You strive for simplicity in your code "
           "that is both concise and easy-to-reason about. "
           "Add comments to the provide code snippet, without changing the code itself."
           "Do not include any headers, titles or explanations outside of the snippet, "
           "keep the three ticks with the language designator (markdown code markup).")))

(defun +replace-region-with-string (replacement)
  "Replace region or buffer content with REPLACEMENT."
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point-min) (point-max)))
  (insert replacement)
  (insert "\n"))

(transient-define-infix +gptel--improve-text-infix-prompt ()
  "Prompt selection for improving text."
  :description "Set prompt"
  :prompt "Prompt: "
  :variable '+gptel-improve-text-prompt
  :class 'transient-lisp-variable
  :key "RET"
  :format "%k %d"
  :reader (lambda (prompt &rest _)
            ;; usual bs to keep the preserve the list order
            (let* ((comp-table (lambda (cs)
                                 (lambda (str pred action)
                                   (if (eq action 'metadata)
                                       `(metadata (display-sort-function . ,#'identity))
                                     (complete-with-action action cs str pred)))))
                   (sel (completing-read
                         prompt
                         (funcall
                          comp-table
                          +gptel-improve-text-prompts-history))))
              (add-to-list '+gptel-improve-text-prompts-history
                           sel)
              sel)))

(require 'gptel-transient)

;;;###autoload
(transient-define-prefix +gptel-improve-text-transient ()
  "Improve region with gptel."
  [:description
   (lambda ()
     (concat
      (or +gptel-improve-text-prompt
          (car +gptel-improve-text-prompts-history)) "\n"))
   [(gptel--infix-provider)
    (+gptel--improve-text-infix-prompt)]
   [""
    ("C-<return>" "Let's go" +gptel-improve-text)]])

;;;###autoload
(defun +gptel-improve-text (&optional arg)
  (interactive "P")
  (unless (region-active-p)
    (user-error "no selection"))
  (setq +gptel-improve-text-prompt (or +gptel-improve-text-prompt
                                       (car +gptel-improve-text-prompts-history)))
  (let ((text (buffer-substring-no-properties
               (region-beginning)
               (region-end))))
    (message "beep-bop... checking your crap with %s" gptel-model)
    (gptel-request text
      :system +gptel-improve-text-prompt
      :callback
      (lambda (resp info)
        (let* ((model (let-plist info .data.model))
               (in-place? (string-match-p
                           "fix mistakes\\|correct mistakes"
                           +gptel-improve-text-prompt)))
          (cond
           (in-place?
            (let* ((_ (+replace-region-with-string resp))
                   (_ (message "¡Ahí está!"))
                   (fst-buf (with-current-buffer (generate-new-buffer (format "* %s 1 *" model))
                              (insert text)
                              (current-buffer)))
                   (snd-buf (with-current-buffer (generate-new-buffer (format "* %s 2 *" model))
                              (insert resp)
                              (current-buffer)))
                   (diff-win (diff fst-buf snd-buf "--text" 'no-async)))

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

           (t
            (let ((buf (generate-new-buffer (format "* %s *" model))))
              (with-current-buffer buf
                (markdown-mode)
                (insert resp))
              (switch-to-buffer-other-window buf)))))))))

;;;###autoload
(defun gptel-clear-buffer+ ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (progn
      (erase-buffer)
      (insert "### ")
      (evil-insert-state))))

;;;###autoload
(defun gptel+ (&optional arg)
  (interactive "P")
  (if arg
      ;; with an argument it tries to create "next" ChatGPT buffer
      ;; containing numerical suffix in the name
      (let ((next-sufx (thread-last
                         (buffer-list)
                         (seq-filter (lambda (b) (string-match-p "^\\*ChatGPT" (buffer-name b))))
                         (seq-map (lambda (b)
                                    (let ((bname (buffer-name b)))
                                      (string-match "^\\*ChatGPT\\(-\\([0-9]+\\)\\)?\\*$" bname)
                                      (if-let ((num-str (match-string 2 bname)))
                                          (string-to-number num-str)
                                        0))))
                         (apply #'max)
                         (funcall (lambda (n) (+ n (if (zerop n) 2 1)))))))
        (switch-to-buffer (gptel (format "*ChatGPT-%s*" next-sufx)))
        (evil-insert-state))
    (switch-to-buffer (gptel "*ChatGPT*"))))
