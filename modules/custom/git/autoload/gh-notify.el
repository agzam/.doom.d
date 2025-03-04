;;; custom/git/autoload/gh-notify.el -*- lexical-binding: t; -*-

(defun gh-notify-notification-read-p (&optional notification)
  (when-let ((obj (gh-notify-notification-forge-obj
                   (or notification
                       (gh-notify-current-notification)))))
    (not (oref obj unread-p))))

;;;###autoload
(defun gh-notify-mark-read-and-move-next (&optional arg)
  "Marks notification read and moves to the next (or prev if ARG)"
  (interactive)
  (let* ((notification (gh-notify-current-notification))
         (forge-obj (gh-notify-notification-forge-obj notification))
         (repo (forge-get-repository forge-obj))
         (topic (forge-get-topic repo (gh-notify-notification-topic notification)))
         (gh-notify-redraw-on-visit nil)
         (_ (gh-notify-set-notification-status notification 'done)))
    (setf (cl-struct-slot-value
           'gh-notify-notification
           'unread notification) nil)
    (with-current-buffer (current-buffer)
      (read-only-mode -1)
      (kill-whole-line)
      (insert (funcall 'gh-notify-render-notification notification))
      (insert "\n")
      (read-only-mode +1)))
  (forward-line (if arg -2 0)))

;;;###autoload
(defun gh-notify-mark-read-and-move-prev ()
  (interactive)
  (funcall-interactively #'gh-notify-mark-read-and-move :prev))

;;;###autoload
(defun gh-notify-code-review-forge-pr-at-point ()
  "Jumps to PR review straight from notications list."
  (interactive)
  (if-let* ((obj (gh-notify-current-notification))
            (pr-p (cl-struct-slot-value 'gh-notify-notification 'type obj))
            (forge-buf (call-interactively #'gh-notify-visit-notification)))
      (with-current-buffer forge-buf
        (run-with-timer
         0.3 nil
         #'code-review-forge-pr-at-point))
    (message "Not a Pull-Request")))

;;;###autoload
(defun gh-notify-forge-browse-topic-at-point ()
  "Browse topic straight from the notifications list."
  (interactive)
  (browse-url
   ;; notification url usually points to api, e.g.:
   ;; https://api.github.com/repos/advthreat/tenzin/pulls/2030,
   ;;
   ;; we need to make it look like: https://github.com/advthreat/tenzin/pulls/2030
   (replace-regexp-in-string
    "\\(pull\\)s" "\\1"
    (replace-regexp-in-string
     "api\\.\\|repos/" ""
     (gh-notify-notification-url
      (gh-notify-current-notification))))))
