;;; custom/ai/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun gptel-clear-buffer+ ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (progn
      (erase-buffer)
      (insert "### ")
      (evil-insert nil))))

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
        (evil-insert 1))
    (switch-to-buffer (gptel "*ChatGPT*"))))
