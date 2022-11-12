;;; custom/general/autoload/narrow.el -*- lexical-binding: t; -*-

(defun clone-indirect-buffer-de-activate-mark ()
  "This is a workaround for the evil visual state error message
like: Error in post-command-hook (evil-visual-post-command):
(error \"Marker points into wrong buffer\" #<marker at 27875 in .spacemacs<2>>)"
  (let ((region-was-active (region-active-p)))
    (when region-was-active (deactivate-mark))
    (call-interactively 'clone-indirect-buffer)
    (when region-was-active (activate-mark))))

(defun narrow-to-indirect-buffer (narrower target-name)
  "Use the function `narrower' to narrow within an indirect
buffer, except where the starting buffer is in a state (such as
visual block mode) that would cause this to work
incorrectly. `target-name' is the string name of the entity being
narrowed to."
  ;; There may be a way to get visual block mode working similar to the
  ;; workaround we did for visual line mode; this usecase however seems like an
  ;; edgecase at best, so let's patch it if we find out it's needed; otherwise
  ;; let's not hold up the base functionality anymore.
  (if (and (eq evil-state 'visual) (eq evil-visual-selection 'block))
      (message "Cannot narrow to indirect buffer from visual block mode.")
    (when evil-ex-active-highlights-alist
      (evil-ex-nohighlight))
    (clone-indirect-buffer-de-activate-mark)
    (call-interactively narrower)
    (message (format "%s narrowed to an indirect buffer" target-name))))

;;;###autoload
(defun narrow-to-defun-indirect-buffer ()
  (interactive)
  (narrow-to-indirect-buffer 'narrow-to-defun "Function"))

;;;###autoload
(defun narrow-to-region-indirect-buffer ()
  (interactive)
  (narrow-to-indirect-buffer 'narrow-to-region "Region"))

;;;###autoload
(defun init-visual-line-keys ()
  (evil-define-minor-mode-key 'motion 'visual-line-mode "j" #'evil-next-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode "k" #'evil-previous-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<down>") #'evil-next-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<up>") #'evil-previous-visual-line))

;;;###autoload
(defun +toggle-visual-line-navigation (&optional arg)
  (interactive)
  (visual-line-mode (or arg 'toggle))
  (if visual-line-mode
      (toggle-truncate-lines -1)
    (toggle-truncate-lines +1))
  (init-visual-line-keys)
  (evil-normalize-keymaps))

(defun shruggie (&optional do-not-escape?)
  (interactive "P")
  (if do-not-escape?
      (insert "¯\\_(ツ)_/¯")
    (insert "¯\\\\\\_(ツ)_/¯")))

(defun diff-last-two-kills (&optional ediff?)
  "Diff last couple of things in the kill-ring. With prefix open ediff."
  (interactive "P")
  (require 'ediff)
  (let* ((old "/tmp/old-kill")
         (new "/tmp/new-kill")
         (prev-ediff-quit-hook ediff-quit-hook))
    (cl-flet ((kill-temps
                ()
                (dolist (f (list old new))
                  (kill-buffer (find-buffer-visiting f)))
                (setq ediff-quit-hook prev-ediff-quit-hook)))
      (with-temp-file new
        (insert (current-kill 0 t)))
      (with-temp-file old
        (insert (current-kill 1 t)))
      (if ediff?
          (progn
            (add-hook 'ediff-quit-hook #'kill-temps)
            (ediff old new))
        (diff old new "-u" t)))))

;;;###autoload
(defun find-in-doom-dir ()
  (interactive)
  (doom/goto-private-config-file)
  (projectile-find-dir))

;; indent on paste
;; from Prelude: https://github.com/bbatsov/prelude
(defun +yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) +doom-yank-indent-threshold)
      (sp-reindent)
    (indent-region beg end nil)))
