;;; custom/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'dired-subtree-remove* "custom/dired/autoload" nil t)
(defun dired-subtree-remove* ()
  (interactive)
  (when (dired-subtree--is-expanded-p)
    (dired-next-line 1))
  (dired-subtree-remove))

;;;###autoload (autoload 'dired-subtree-remove* "custom/dired/autoload" nil t)
(defun dired-subtree-down-n-open ()
  (interactive)
  (save-excursion (dired-subtree-insert))
  (when (or (dired-subtree--is-expanded-p)
            (not (eq
                  (point)
                  (save-excursion (dired-subtree-end)
                                  (point)))))
    (dired-next-line 1)))

;;;###autoload
(defun buffer-with-dired-item ()
  "Creates buffer with current item of dired or direx buffer"
  (cond ((eq major-mode 'direx:direx-mode) (-> (direx:item-at-point!)
                                               direx:item-tree
                                               direx:file-full-name
                                               find-file-noselect))
        ((eq major-mode 'dired-mode) (find-file-noselect (dired-get-file-for-visit)))))

;;;###autoload
(defmacro dired-split-action (split-type)
  `(defun ,(intern (concat "dired--" (symbol-name split-type))) ()
     (interactive)
     (let ((buf (buffer-with-dired-item)))
       (funcall #',split-type)
       (switch-to-buffer buf))))

;;;###autoload
(defun dired-ace-action ()
  (interactive)
  (with-demoted-errors "%s"
    (require 'ace-window)
    (let ((buf (buffer-with-dired-item))
          (aw-dispatch-always t))
      (aw-switch-to-window (aw-select nil))
      (switch-to-buffer buf))))

;;;###autoload
(defun treemacs-project-toggle+ ()
  "Toggle and add the current project to treemacs if not already added."
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))
    (let ((path (projectile-ensure-project (projectile-project-root)))
          (name (projectile-project-name)))
      (unless (treemacs-current-workspace)
        (treemacs--find-workspace))
      (treemacs-do-add-project-to-workspace path name)
      (treemacs-display-current-project-exclusively)
      (treemacs-select-window))))

;;;###autoload
(defun +dired-jump-find-in-project ()
  "Buffer file in subtree relative to projects root."
  (interactive)
  (let* ((root (projectile-acquire-root))
         (fname buffer-file-name)
         (parts (when fname (split-string (string-replace root "" fname) "/"))))
    (dired root)
    (when parts
      (goto-char (point-min))
      ;; find initial dir or file
      (dired-goto-file (concat root (car parts)))
      (dolist (part parts)
        (let* ((ov (caddr dired-subtree-overlays)) ; last overlay
               (bound (when ov (overlay-end ov)))) ; search within overlay bounds
          (search-forward part bound :noerror)
          (dired-subtree-insert))))
    (recenter)))

;;;###autoload
(defun treemacs-icons-after-subtree-insert-a ()
  (let ((end (overlay-end (dired-subtree--get-ov))))
    (treemacs-with-writable-buffer
     (save-excursion
       (goto-char (point))
       (dired-goto-next-file)
       (while (< (point) end)
         (if (dired-move-to-filename nil)
             (let* ((file (dired-get-filename nil t))
                    (icon (if (file-directory-p file)
                              treemacs-icon-dir-closed
                            (treemacs-icon-for-file file))))
               (insert icon))
           (treemacs-return nil))
         (forward-line 1))))))
