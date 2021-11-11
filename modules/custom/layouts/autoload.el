;;; custom/layouts/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defhydra +hydra/layouts (:hint nil :color red)
  "

"
  ("p" tab-bar-switch-to-prev-tab)
  ("n" tab-bar-switch-to-next-tab)
  ("N" +tab-bar-add-new-tab)
  ("t" +tab-bar-add-new-tab)
  ("d" +tab-bar-kill-tab)
  ("r" +tab-bar-rename-tab))

(defvar tab-bar-tab-added-hook nil)
(defvar tab-bar-tab-removed-hook nil)

(defun +tab-bar-rename-dups (&optional tabs)
  "Renames tabs with identical names by attaching a numerical suffix."
  (->> (or tabs (tab-bar-tabs))
       (-group-by (lambda (t)
                    ;; ignore num suffix
                    (replace-regexp-in-string
                     " [[:digit:]]*$" ""
                     (alist-get 'name t))))
   (-separate (lambda (x) (< 1 (length (cdr x)))))
   (-map
    (lambda (group)
      (pcase-let ((`((,name . ,tabs)) group))
        (when (< 1 (length tabs))
          (->>
           tabs
           (-drop 1)
           (seq-do-indexed
            (lambda (tab i)
              (let ((name (replace-regexp-in-string
                           " [[:digit:]]*$" ""
                           (alist-get 'name tab))))
                (setf (alist-get 'name tab)
                      (format
                       "%s %s"
                       name
                       (number-to-string (+ 2 i))))
                (setf (alist-get 'explicit-name tab) t)))))))))))

;;;###autoload
(defun +tab-bar-add-new-tab ()
  (interactive)
  (tab-bar-new-tab)
  (+tab-bar-rename-dups)
  (run-hooks 'tab-bar-tab-added-hook))

;;;###autoload
(defun +tab-bar-kill-tab ()
  (interactive)
  (tab-bar-close-tab)
  (+tab-bar-rename-dups)
  (run-hooks 'tab-bar-tab-removed-hook))

;;;###autoload
(defun +tab-bar-rename-tab ()
  (interactive)
  (call-interactively 'tab-bar-rename-tab)
  (+tab-bar-rename-dups))

;;;###autoload
(defun +tab-bar-name-fn ()
  (projectile-project-name))

