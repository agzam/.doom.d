;;; custom/git/autoload/code-review.el -*- lexical-binding: t; -*-

;;;###autoload
(defun code-review-browse-pr+ ()
  (interactive)
  (browse-url
   (let-alist (code-review-db-get-pr-alist)
     (format "https://github.com/%s/%s/pull/%s" .owner .repo .num))))

;;;###autoload
(defadvice! code-review-forge-pr-at-point-no-debug-a (orig-fn &rest args)
  ;; some weird fucking update keeps popping up ghub-debug buffers
  ;; super annoying
  :around #'code-review--build-buffer
  (let ((buf-regx "\\*http api\\.github\\.com:443\\*"))
    (add-to-list 'display-buffer-alist
                 `(,buf-regx
                   (display-buffer-no-window)
                   (allow-no-window . t)))
    (apply orig-fn args)
    ;; let's make sure we can investigate the log buffers later
    (run-with-timer
     5 nil
     (lambda ()
       (setq display-buffer-alist
             (assoc-delete-all buf-regx display-buffer-alist))))))
