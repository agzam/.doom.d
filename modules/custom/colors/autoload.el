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
  (let ((theme (ring-next (colors/init-themes-ring) doom-theme)))
    (load-theme theme :no-confirm)
    theme))

;;;###autoload (autoload 'colors/load-prev-theme "custom/colors/autoload" nil t)
(defun colors/load-prev-theme ()
  (interactive)
  (let ((theme (ring-previous (colors/init-themes-ring) doom-theme)))
    (load-theme theme :no-confirm)
    theme))

;;;###autoload (autoload 'colors/cycle-themes/body "custom/colors/autoload" nil t)
(defhydra colors/cycle-themes (:hint nil :color red)
  "
     Load theme _n_:next, _p_:previous, _l_:list themes
"
  ("n" colors/load-next-theme "next")
  ("p" colors/load-prev-theme "previous")
  ("l" consult-theme "list themes" :exit t))
