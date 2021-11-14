;;; custom/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'direx/jump-to-project-root-or-current-dir "custom/dired/autoload" nil t)
(defun direx/jump-to-project-root-or-current-dir ()
  "Open in Direx - project root if there's one, otherwise current directory."
  (interactive)
  (let ((buf (direx-project:find-project-root-noselect
              (or buffer-file-name default-directory))))
    (if buf
        (progn
          (direx:maybe-goto-current-buffer-item buf)
          (switch-to-buffer buf))
      (direx:find-directory "."))))

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

(defun buffer-with-dired-item ()
  "Creates buffer with current item of dired or direx buffer"
  (cond ((eq major-mode 'direx:direx-mode) (-> (direx:item-at-point!)
                                               direx:item-tree
                                               direx:file-full-name
                                               find-file-noselect))
        ((eq major-mode 'dired-mode) (find-file-noselect (dired-get-file-for-visit)))))

(setq split-ace-window--current nil)

(defun ace-window-set-side (side)
  (interactive)
  (setq split-ace-window--current
        (plist-put
         split-ace-window--current
         :side side)))

(defhydra +hydra/split-ace-window
  (:color amarath
   :hint nil
   :before-exit ((lambda ()
                   (let* ((win (plist-get split-ace-window--current :window))
                          (side (plist-get split-ace-window--current :side)))
                     (select-window (split-window win nil side t))
                     (switch-to-buffer
                      (plist-get split-ace-window--current :buffer))
                     (setq split-ace-window--current nil)))))
  "
Split window

     ↑
     _k_
← _h_  ·  _l_ →
     _j_
     ↓
"
  ("h" (ace-window-set-side 'left) :exit t)
  ("l" (ace-window-set-side 'right) :exit t)
  ("j" (ace-window-set-side 'below) :exit t)
  ("k" (ace-window-set-side 'above) :exit t))

;;;###autoload
(defun visit-file-ace-window ()
  (interactive)
  (let* ((dired-item-buf (buffer-with-dired-item))
         (win (aw-select "Select window")))
    (setq split-ace-window--current
          (plist-put split-ace-window--current :window win))
    (setq split-ace-window--current
          (plist-put split-ace-window--current :buffer dired-item-buf))
    (+hydra/split-ace-window/body)))
