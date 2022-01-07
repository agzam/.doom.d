;;; custom/tab-bar/autoload.el -*- lexical-binding: t; -*-

(require 'projectile)

;;;###autoload
(defhydra +hydra/tab-bar (:hint nil :color red)
  "
Layouts
---------------------------------
_p_: prev                 _t_: new tab
_n_: next                 _d_: kill tab
_<_: move left            _r_: rename
_>_: move right           _l_: select
_b_: move buffer to tab   _w_: separate window into new tab
_f_: find tab with buffer
"
  ("p" tab-bar-switch-to-prev-tab)
  ("n" tab-bar-switch-to-next-tab)
  ("N" +tab-bar-add-new-tab)
  ("t" +tab-bar-add-new-tab)
  ("d" +tab-bar-kill-tab)
  ("r" +tab-bar-rename-tab)
  ("<" +tab-bar-tab-move-left)
  (">" +tab-bar-tab-move-right)
  ("l" tab-bar-select-tab-by-name :exit t)
  ("b" +tab-bar-move-buffer-to-tab :exit t)
  ("f" +tab-bar-find-buffer-in-tabs :exit t)
  ("w" tab-bar-move-window-to-tab :exit t)
  ("1" (+tab-bar-switch-to-tab-number 1))
  ("2" (+tab-bar-switch-to-tab-number 2))
  ("3" (+tab-bar-switch-to-tab-number 3))
  ("4" (+tab-bar-switch-to-tab-number 4))
  ("5" (+tab-bar-switch-to-tab-number 5))
  ("6" (+tab-bar-switch-to-tab-number 6))
  ("7" (+tab-bar-switch-to-tab-number 7))
  ("8" (+tab-bar-switch-to-tab-number 8))
  ("9" (+tab-bar-switch-to-tab-number 9))
  ("0" (+tab-bar-switch-to-tab-number 'last)))

;;;###autoload
(defvar tab-bar-tab-added-hook nil)

;;;###autoload
(defvar tab-bar-tab-removed-hook nil)

;;;###autoload
(defun +tab-bar-switch-to-tab-number (num)
  (interactive)
  (if (eq 'last num)
      (tab-bar-select-tab (length (tab-bar-tabs)))
      (tab-bar-select-tab num)))

(defun +tab-bar-rename-dups (&optional tabs)
  "Renames tabs with identical names by attaching a numerical suffix."
  (->> (or tabs (tab-bar-tabs))
       (-group-by (lambda (tab)
                    ;; ignore num suffix
                    (replace-regexp-in-string
                     " [[:digit:]]*$" ""
                     (alist-get 'name tab))))
   (-separate (lambda (x) (< 1 (length (cdr x)))))
   (-map
    (lambda (group)
      (pcase-let ((`((,name . ,tabs)) group))
        (when (< 1 (length tabs))
          (->>
           tabs
           ;; (-drop 1)
           (seq-do-indexed
            (lambda (tab i)
              (let ((name (replace-regexp-in-string
                           " [[:digit:]]*$" ""
                           (alist-get 'name tab))))
                (setf (alist-get 'name tab)
                      (concat
                       name
                       (when (< 0 i)
                         (concat " " (number-to-string (+ 1 i))))))
                (setf (alist-get 'explicit-name tab) t)))))))))))

;;;###autoload
(defun +tab-bar-tab-move-left ()
  (interactive)
  (tab-bar-move-tab -1))

;;;###autoload
(defun +tab-bar-tab-move-right ()
  (interactive)
  (tab-bar-move-tab 1))

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
  (run-with-timer "0.3 sec" nil #'+tab-bar-rename-dups)
  (run-hooks 'tab-bar-tab-removed-hook))

;;;###autoload
(defun +tab-bar-rename-tab ()
  (interactive)
  (call-interactively 'tab-bar-rename-tab)
  (+tab-bar-rename-dups))

;;;###autoload
(defun +tab-bar-name-fn ()
  (let* ((buf-fname (buffer-file-name))
         (buf-dir (when buf-fname (file-name-directory buf-fname))))
    (cond
     ((eq 'dired-mode major-mode)
      (projectile-project-name (projectile-project-root default-directory)))

     ((and buf-dir (projectile-project-p buf-dir))
      (projectile-project-name (projectile-project-root buf-dir)))

     (buf-dir buf-dir)

     ((not (string-match-p "\\*Minibuf" (buffer-name)))
      (buffer-name)))))

;;;###autoload
(defun tab-bar-created-h ()
  (doom/switch-to-scratch-buffer)
  (let* ((completion-fn (lambda (prompt lst)
                          (condition-case _ (completing-read prompt lst)
                            (quit nil))))
         (proj-dir (funcall completion-fn "choose project:" projectile-known-projects)))
    (if-let* (proj-dir (dir-buf (find-file-noselect proj-dir)))
        (switch-to-buffer dir-buf)
      (switch-to-buffer
       (find-file-noselect
        (funcall
         completion-fn "choose dir:"
         (split-string (shell-command-to-string "fasd -lRd")))))))
  (tab-bar-rename-tab nil)
  (+tab-bar-rename-dups))

;;;###autoload
(defun +tab-bar-move-buffer-to-tab ()
  (interactive)
  (let ((buf (current-buffer))
        (pos (point)))
    (if (length (window-list))
        (bury-buffer)
      (delete-window))
    (call-interactively 'tab-bar-select-tab-by-name)
    (split-window-sensibly)
    (switch-to-buffer buf)
    (goto-char pos)))

;;;###autoload
(defun +tab-bar-find-buffer-in-tabs ()
  "Find and switch to the tab that owns selected buffer."
  (interactive)
  (let ((sel-buf nil))
    (cl-letf (((symbol-function 'consult--buffer-action)
               (lambda (b) (setq sel-buf b))))
      (consult-buffer)
      (when-let* ((tab (tab-bar-get-buffer-tab sel-buf)))
        (if (eq 'current-tab (car tab))
            (select-window (get-buffer-window sel-buf))
          (tab-bar-switch-to-tab (alist-get 'name tab)))))))
