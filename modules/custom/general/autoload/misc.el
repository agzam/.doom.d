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
  (let ((old-buffer (generate-new-buffer " *old-kill*"))
        (new-buffer (generate-new-buffer " *new-kill*")))
    (with-current-buffer new-buffer
      (insert (current-kill 0 t)))
    (with-current-buffer old-buffer
      (insert (current-kill 1 t)))
    (if ediff?
        (ediff-buffers old-buffer new-buffer)
      (diff old-buffer new-buffer nil t))))

;;;###autoload
(defun find-in-doom-dir ()
  (interactive)
  (find-file doom-user-dir)
  (projectile-find-dir))

;; indent on paste
;; from Prelude: https://github.com/bbatsov/prelude
(defun +yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) +doom-yank-indent-threshold)
      (ignore-errors (sp-reindent))
    (indent-region beg end nil)))


;;;###autoload
(defun +doom-remove-package-artifacts ()
  "Remove repo and build file for a selected package.
Useful for clean up before running `doom -up`."
  (interactive)
  (ignore-errors (require 'straight))
  (let* ((sel-dir (completing-read
                   "Select package repo: "
                   (directory-files (straight--repos-dir ))))
         (dirs (list (straight--repos-dir sel-dir)
                     (straight--build-dir sel-dir))))
    (dolist (d dirs)
      (delete-directory d :recursive))))

;;;###autoload
(defun yas-completing-prompt (prompt choices &optional display-fn completion-fn)
  "Overriding yas-completing-prompt with my own version that doesn't require-match."
  (let* ((formatted-choices
          (if display-fn (mapcar display-fn choices) choices))
         (chosen (funcall (or completion-fn #'completing-read)
                          prompt formatted-choices
                          nil nil nil nil)))
    (if (eq choices formatted-choices)
        chosen
      (nth (or (cl-position chosen formatted-choices :test #'string=) 0)
           choices))))

;;;###autoload
(defun auth-host->pass (host)
  (auth-source-pick-first-password :host host))
