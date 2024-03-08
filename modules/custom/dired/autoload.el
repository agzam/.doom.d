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
  (let* ((root (or (projectile-project-root) default-directory))
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
                            (treemacs-icon-for-file file)))
                    ;; check if there's already an icon in the current line
                    (icon? (save-excursion
                             (goto-char (line-end-position))
                             (re-search-backward "  \\s-*" (line-beginning-position) t)
                             (get-text-property (point) 'display))))
               (unless icon? (insert icon)))
           (treemacs-return nil))
         (forward-line 1))))))

;;;###autoload
(defun dired-file-to-mplayer (&optional filename)
  (interactive)
  (let ((filename (or filename (dired-get-file-for-visit))))
    (unless (get-buffer "*vterm*") (vterm))
    (switch-to-buffer-other-window "*vterm*")
    (vterm-send-string
     (concat
      "mplayer -af scaletempo=scale=1.3:speed=tempo -volume 80 -speed 1.3 "
      "\"" (expand-file-name filename) "\""))
    (vterm-send-return)))
