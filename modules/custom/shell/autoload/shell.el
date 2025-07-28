;;; custom/shell/autoload/shell.el -*- lexical-binding: t; -*-

;;;###autoload
(defun shell-pop-choose (&optional arg)
  (interactive "P")
  (let* ((shell-type (completing-read "Shell: " '(eshell vterm shell)))
         (shell-fn (pcase shell-type
                     ("eshell" #'eshell)
                     ("vterm" (lambda () (vterm)))
                     ("shell" #'shell))))
    (shell-pop--set-shell-type
     'shell-pop-shell-type
     `(,shell-type
       ,(format "*%s*" shell-type)
       (lambda () (,shell-fn))))
    (shell-pop arg)))


;;;###autoload
(defun shell-pop-in-project-root (&optional arg)
  (interactive)
  (if-let ((pr (projectile-project-root)))
      (projectile-with-default-dir pr
          (shell-pop arg))
    (shell-pop arg)))

;;;###autoload
(defun update-hyprland-env-h ()
  "Update Hyprland/Wayland environment variables for the current session.
This ensures Emacs has the correct environment variables for Wayland
operations, useful for long-running Emacs sessions or when launched
from different contexts. Only runs on Linux systems with Hyprland."
  (when (eq system-type 'gnu/linux)
    ;; Find and update HYPRLAND_INSTANCE_SIGNATURE
    (when-let* ((runtime-dir (getenv "XDG_RUNTIME_DIR"))
                (hypr-dir (expand-file-name "hypr" runtime-dir))
                (instances (and (file-directory-p hypr-dir)
                                (directory-files hypr-dir nil "^[^.]" t)))
                (signature (car instances)))
      (setenv "HYPRLAND_INSTANCE_SIGNATURE" signature)

      ;; Update WAYLAND_DISPLAY if socket exists
      (when-let ((wayland-sock (car (file-expand-wildcards
                                     (concat runtime-dir "/wayland-*") t))))
        (setenv "WAYLAND_DISPLAY" (file-name-nondirectory wayland-sock)))

      ;; Update Hyprland IPC socket
      (let ((hypr-sock (expand-file-name ".socket2.sock"
                                         (expand-file-name signature hypr-dir))))
        (when (file-exists-p hypr-sock)
          (setenv "HYPRLAND_IPC" hypr-sock)))

      ;; Ensure XDG session type is set correctly
      (setenv "XDG_SESSION_TYPE" "wayland")

      ;; Update XDG current desktop if not set
      (unless (getenv "XDG_CURRENT_DESKTOP")
        (setenv "XDG_CURRENT_DESKTOP" "Hyprland")))))
