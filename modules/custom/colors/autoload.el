;;; custom/colors/autoload.el -*- lexical-binding: t; -*-

(defvar colors--themes-ring nil)

(defun colors/init-themes-ring ()
  (when (null colors--themes-ring)
    (setq colors--themes-ring
          (ring-convert-sequence-to-ring
           (seq-map 'cdr circadian-themes))))
  colors--themes-ring)

;;;###autoload (autoload 'colors/load-next-theme "custom/colors/autoload" nil t)
(defun colors/load-next-theme ()
  (interactive)
  (let* ((ring (colors/init-themes-ring))
         (theme (progn
                  (unless (ring-member ring doom-theme)
                    (ring-extend ring 1)
                    (ring-insert ring doom-theme))
                  (ring-next ring doom-theme))))
    ;; clean up face definitions from the previous theme so the colors
    ;; don't "leak" to another
    (mapc #'disable-theme custom-enabled-themes)
    (setq doom-theme theme)
    (load-theme theme :no-confirm)
    theme))

;;;###autoload (autoload 'colors/load-prev-theme "custom/colors/autoload" nil t)
(defun colors/load-prev-theme ()
  (interactive)
  (let* ((ring (colors/init-themes-ring))
         (theme (progn
                  (unless (ring-member ring doom-theme)
                    (ring-extend ring 1)
                    (ring-insert ring doom-theme))
                  (ring-previous ring doom-theme))))
    ;; clean up face definitions from the previous theme so the colors
    ;; don't "leak" to another
    (mapc #'disable-theme custom-enabled-themes)
    (setq doom-theme theme)
    (load-theme theme :no-confirm)
    theme))

;;;###autoload
(transient-define-prefix colors/cycle-themes ()
  "toggle theme"
  ["Load theme"
   [("n" "next" colors/load-next-theme :transient t)
    ("p" "previous" colors/load-prev-theme :transient t)
    ("l" "list themes" consult-theme)]])

;;;###autoload
(defun colors/cycle-themes-up ()
  (interactive)
  (colors/load-prev-theme)
  (circadian-setup)
  (colors/cycle-themes))

;;;###autoload
(defun colors/cycle-themes-down ()
  (interactive)
  (colors/load-next-theme)
  (colors/cycle-themes))
