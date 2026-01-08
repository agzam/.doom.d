;;; custom/pdf/autoload/nov.el -*- lexical-binding: t; -*-

;;;###autoload
(defun nov-back-or-quit ()
  (interactive)
  (if nov-history
      (nov-history-back)
    (kill-current-buffer)))

(defun nov--text-scale-adjust (inc)
  "Adjust text scale in nov-mode buffer by remapping shr faces."
  ;; First, scale the default face (standard behavior)
  (text-scale-increase inc)
  ;; Then remap all the shr-* faces that nov uses
  (let ((scale-factor (expt text-scale-mode-step text-scale-mode-amount)))
    (dolist (face '(shr-text shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6 
                    shr-code shr-strike shr-mark shr-italic shr-bold
                    shr-link shr-preformatted))
      (face-remap-add-relative face :height scale-factor))))

;;;###autoload
(defun nov-text-scale-increase ()
  (interactive)
  (nov--text-scale-adjust 0.5))

;;;###autoload
(defun nov-text-scale-decrease ()
  (interactive)
  (nov--text-scale-adjust -0.5))
