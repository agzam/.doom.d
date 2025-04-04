;;; custom/tab-bar/autoload.el -*- lexical-binding: t; -*-

(require 'projectile)

;;;###autoload
(defvar tab-bar-tab-added-hook nil)
(defvar tab-bar-tab-removed-hook nil)

;;;###autoload
(transient-define-prefix tab-bar-new-tab-transient ()
  "New Tab"
  ["Choose a template"
   [("o" "Org" (lambda () (interactive) (org-roam-dailies-goto-today '("w"))))
    ("gt" "gptel" gptel+)
    ("gn" "gh-notify" gh-notify)]

   [("ef" "elfeed" elfeed)
    ("no" "notmuch" notmuch)
    ("t" "telega" telega)]

   [("ed" "doom.d" find-in-doom-dir)
    ("ei" "emacs.d" (lambda () (interactive) (dired (file-name-directory doom-emacs-dir))))
    ("D" "dotfile.org" (lambda () (interactive) (find-file "~/dotfile.org/dotfile.org")))]

   [("hn" "NH" hnreader-news)
    ("rd" "Reddit" reddigg-view-frontpage)]

   [("p" "projects" (lambda ()
                      (interactive)
                      (switch-to-buffer
                       (find-file-noselect
                        (completing-read "choose project: " projectile-known-projects)))))
    ("SPC" "zoxide history" +zoxide-find)
    ("b" "buffers" consult-buffer)]
   [("d" "kill tab" +tab-bar-kill-tab)]])

;;;###autoload
(transient-define-prefix tab-bar-transient ()
  "Layouts"
  ["Layouts"
   ["" "" "" ""
    ("<tab>" "recent" tab-bar-switch-to-recent-tab)]
   [("k" "prev" tab-bar-switch-to-prev-tab)
    ("j" "next" tab-bar-switch-to-next-tab)
    ("<" "move left" +tab-bar-tab-move-left :transient t)
    (">" "move right" +tab-bar-tab-move-right :transient t)
    ("w" "move window to new tab" tab-bar-move-window-to-tab)]
   [("t" "new tab" +tab-bar-add-new-tab)
    ("n" "new tab" +tab-bar-add-new-tab)
    ("D" "Duplicate" +tab-bar-duplicate-tab)
    ("r" "rename" +tab-bar-rename-tab)
    ("l" "select" tab-bar-select-tab-by-name)]
   [("b" "move buffer to tab" +tab-bar-move-buffer-to-tab)
    ("f" "find tab with current buffer" +tab-bar-find-buffer-in-tabs)
    ("K" "kill project buffers" +tab-bar-kill-project-buffers)]
   [("[" "history back" tab-bar-history-back :transient t)
    ("]" "history forward" tab-bar-history-forward :transient t)
    ("d" "kill tab" +tab-bar-kill-tab)
    ("u" "undo kill tab" tab-undo)
    ("SPC" "templates" tab-bar-new-tab-transient)]]
  [:hide always
   :class transient-columns
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      'tab-bar-transient
      (mapcar
       (lambda (n)
         (list (number-to-string n)
               (format "Goto: %s" n)
               (lambda ()
                 (interactive)
                 (+tab-bar-switch-to-tab-number n))))
       (number-sequence 1 9))))])

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
                    (when-let ((tname (alist-get 'name tab)))
                      (replace-regexp-in-string
                       " [[:digit:]]*$" "" tname))))
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

(add-hook! 'tab-bar-tab-added-hook
  (defun tab-bar-template-run-hook-h ()
    (tab-bar-rename-tab nil)
    (+tab-bar-rename-dups)))

;;;###autoload
(defun +tab-bar-add-new-tab ()
  (interactive)
  (let ((tab-bar-new-tab-to 'rightmost))
    (tab-bar-new-tab))
  (doom/switch-to-scratch-buffer)
  (tab-bar-new-tab-transient))

;;;###autoload
(defun +tab-bar-kill-tab ()
  (interactive)
  (tab-bar-close-tab)
  (run-with-timer "0.3 sec" nil #'+tab-bar-rename-dups)
  (run-hooks 'tab-bar-tab-removed-hook))

;;;###autoload
(defun +tab-bar-duplicate-tab ()
  (interactive)
  (tab-bar-duplicate-tab)
  (+tab-bar-rename-dups))

;;;###autoload
(defun +tab-bar-rename-tab ()
  (interactive)
  (call-interactively 'tab-bar-rename-tab)
  (+tab-bar-rename-dups))

;;;###autoload
(defun +tab-bar-name-fn ()
  (require 'magit)
  (let* ((project-name (projectile-project-name))
         (buf-fname (buffer-file-name))
         (buf-name (buffer-name))
         (buf-dir (when buf-fname (file-name-directory buf-fname)))
         (branch (when (or buf-fname
                           (eq major-mode 'dired-mode))
                   (magit-get-current-branch)))
         (check-fn (lambda (opt)
                     (let ((rev-parse-res (or (magit-rev-parse opt) "")))
                       (if (string= rev-parse-res ".") t
                         (string= ".git" (substring rev-parse-res -4))))))
         (worktree? (and branch
                         (not (and (funcall check-fn "--git-dir")
                                   (funcall check-fn "--git-common-dir")))))
         (label (cond
                 ((member major-mode '(gh-notify-mode)) buf-name)

                 (worktree?
                  (file-name-nondirectory
                   (expand-file-name ".." (projectile-project-root))))

                 ((and branch (not worktree?))
                  (file-name-nondirectory
                   (expand-file-name "." (projectile-project-root))))

                 ;; ((and branch
                       ;; (not (string-equal "-" project-name)))
                  ;; project-name)

                 ;; ((eq 'dired-mode major-mode)
                 ;;  (projectile-project-name (projectile-project-root default-directory)))

                 ((and buf-dir (projectile-project-p buf-dir))
                  (projectile-project-name (projectile-project-root buf-dir)))

                 (buf-dir buf-dir)

                 ((not (string-match-p "\\*Minibuf" buf-name))
                  buf-name))))
    (concat label (when branch (format "󠀠 ▸ %s" branch)))))

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
(defun +tab-bar-kill-project-buffers ()
  (interactive)
  (when (featurep 'project)
    (project-kill-buffers)
    (+tab-bar-kill-tab)))

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
