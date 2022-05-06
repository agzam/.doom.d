;;; custom/version-control/autoload/gh-notify.el -*- lexical-binding: t; -*-

(defun gh-notify-notification-read-p (&optional notification)
  (when-let ((obj (gh-notify-notification-forge-obj
                   (or notification
                       (gh-notify-current-notification)))))
    (not (oref obj unread-p))))

;;;###autoload
(defun gh-notify-mark-read-and-move ()
  "Marks notification and moves either up or down, depending on the status of next notification."
  (interactive)
  (let* ((notification (gh-notify-current-notification))
         (gh-notify-redraw-on-visit nil)
         (_ (gh-notify-mark-notification-read notification)))
    (setf (cl-struct-slot-value
           'gh-notify-notification
           'unread notification) nil)
    (with-current-buffer (current-buffer)
      (read-only-mode -1)
      (delete-line)
      (insert (funcall 'gh-notify-render-notification notification))
      (insert "\n")
      (read-only-mode +1)))
  (when (gh-notify-notification-read-p)
    (forward-line -2)))

;;;###autoload
(defun gh-notify-code-review-forge-pr-at-point ()
  "Jumps to PR review straight from notications list."
  (interactive)
  (if-let* ((obj (gh-notify-current-notification))
            (pr-p (eq 'pullreq (oref obj type)))
            (forge-buf (call-interactively #'gh-notify-visit-notification)))
      (with-current-buffer forge-buf
        (run-with-timer
         0.3 nil
         #'code-review-forge-pr-at-point))
    (message "Not a Pull-Request")))
