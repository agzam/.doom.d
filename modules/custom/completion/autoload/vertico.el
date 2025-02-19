;;; custom/completion/autoload/vertico.el -*- lexical-binding: t; -*-

(defvar vertico-posframe-global nil)

(defvar vertico-posframe-tall-mode nil
  "Track if vertico-posframe is in tall mode.")

(defun vertico-posframe-height-restore-h ()
  (setq vertico-posframe-height nil
        vertico-count 15
        vertico-posframe-tall-mode nil))

;;;###autoload
(defun vertico-posframe-briefly-tall ()
  (interactive)
  (if vertico-posframe-tall-mode
      (vertico-posframe-height-restore-h)
    (let* ((frame-height (frame-height))
           (max-height (- frame-height 2))
           (vertico-height (min 75 max-height))
           (count (- vertico-height 18)))
      (setq vertico-posframe-height vertico-height
            vertico-count count
            vertico-posframe-tall-mode t)
      (add-hook! 'minibuffer-exit-hook #'vertico-posframe-height-restore-h))))

;;;###autoload
(defun vertico-jump-to-home-dir-on~  ()
  "Puts you in home dir initial location in find-file"
  (interactive)
  (when (eq 'file (vertico--metadata-get 'category))
    (vertico-directory-up 1000)
    (unless (string-match-p "^~.*" (minibuffer-contents))
      (beginning-of-line)
      (insert "~")
      (end-of-line))))

;;;###autoload
(defun vertico-jump-root  ()
  "Puts you in root dir initial location in find-file"
  (interactive)
  (when (eq 'file (vertico--metadata-get 'category))
    (vertico-directory-up 1000)
    (vertico-directory-up 1000)))

;;;###autoload
(defun vertico-jump-sudo  ()
  "Puts you in /sudo::/ initial location in find-file"
  (interactive)
  (when (eq 'file (vertico--metadata-get 'category))
    (vertico-directory-up 1000)
    (vertico-directory-up 1000)
    (insert "sudo::/")
    (end-of-line)))
