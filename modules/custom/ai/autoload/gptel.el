;;; custom/ai/gptel.el -*- lexical-binding: t; -*-
(defvar +gptel-improve-text-prompt nil)

(defvar +gptel-improve-text-prompts-history
  `("Only correct mistakes, do not alter the text"
    "Fix mistakes and flag factual inaccuracies, do not alter the text structure"
    ,(concat
      "Provide 3 different improved variations of the given text, separating each variation with:"
      "\n\n---\n\n"
      "Do not include any explanations, titles, headers or bullet points - ONLY plain text of variants, nothing else!")
    "Explain the code snippet"
    "Add comments to the code snippet"))

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
      :system (concat
               +gptel-improve-text-prompt
               " Exclude any explanations - response must contain ONLY the altered text,"
               " or nothing if there were no changes.")
      :callback
      (lambda (resp info)
        (let* ((model (let-plist info .data.model))
               (multi? (string-match-p
                        "variations\\|versions"
                        +gptel-improve-text-prompt)))
          (cond
           (multi?
            (with-current-buffer (generate-new-buffer (format " * %s *" model))
              (markdown-mode)
              (insert resp)
              (switch-to-buffer-other-window (current-buffer))))

           (t
            (let* ((_ (+replace-region-with-string resp))
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
              (kill-buffer snd-buf)))))))))

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
