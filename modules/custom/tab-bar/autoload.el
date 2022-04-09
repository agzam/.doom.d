;;; custom/tab-bar/autoload.el -*- lexical-binding: t; -*-

(require 'projectile)


;;;###autoload
(defvar tab-bar-tab-added-hook nil)
(defvar tab-bar-tab-removed-hook nil)

;;;###autoload
(defvar tab-bar--templates
  '(("o" "Org" (progn
                 (require 'org)
                 (find-file (concat (file-name-directory org-directory) "planning.org"))))
    ("ed" "doom.d" (doom/goto-private-config-file))
    ("ei" "emacs.d" (dired doom-emacs-dir))
    ("p" "projects" (switch-to-buffer
                     (find-file-noselect
                      (completing-read "choose project:" projectile-known-projects))))
    ("fh" "fasd history"
     (switch-to-buffer
      (find-file-noselect
       (completing-read "choose dir:" (split-string (shell-command-to-string "fasd -lRd")))))))
  "List of built-in templates for new tabs
 For user-customizable templates use `tab-bar-custom-templates'")

;;;###autoload
(defun tab-bar--update-templates ()
  (require 'transient)
  (transient-define-prefix new-tab-transient ()
    "New Tab"
    ["Choose a template\n"
     [,@(seq-map
         (lambda (x)
           (pcase-let ((`(,key ,desc ,form) x))
             `(,key ,desc (lambda ()
                            (interactive)
                            ,form
                            (run-hooks 'tab-bar-tab-added-hook)))))
         tab-bar--templates)]
     [("d" "kill tab" +tab-bar-kill-tab)]]))

;;;###autoload
(require 'transient)
;;;###autoload
(transient-define-prefix tab-bar-transient ()
  "Layouts"
  ["Layouts\n"
   [("k" "prev" tab-bar-switch-to-prev-tab)
    ("j" "next" tab-bar-switch-to-next-tab)
    ("<" "move left" +tab-bar-tab-move-left :transient t)
    (">" "move right" +tab-bar-tab-move-right :transient t)
    ("b" "move buffer to tab" +tab-bar-move-buffer-to-tab)
    ("f" "find tab with current buffer" +tab-bar-find-buffer-in-tabs)
    ("w" "move window to new tab" tab-bar-move-window-to-tab)]
   [("t" "new tab" +tab-bar-add-new-tab)
    ("n" "new tab" +tab-bar-add-new-tab)
    ("d" "kill tab" +tab-bar-kill-tab)
    ("r" "rename" +tab-bar-rename-tab)
    ("l" "select" tab-bar-select-tab-by-name)]
   [,@(seq-map
       (lambda (n)
         (let ((snum (number-to-string n)))
           `(,snum ,(format "Goto: %s" n) (lambda () (interactive) (+tab-bar-switch-to-tab-number ,n)))))
       (number-sequence 1 9))]])

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
  (tab-bar--update-templates)
  (tab-bar-new-tab)
  (doom/switch-to-scratch-buffer)
  (new-tab-transient))

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
