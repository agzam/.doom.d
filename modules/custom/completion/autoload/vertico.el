;;; custom/completion/autoload/vertico.el -*- lexical-binding: t; -*-

(defvar vertico-posframe-global nil)

;;;###autoload
(defun vertico-posframe-briefly-tall ()
  (interactive)
  (setq vertico-posframe-height 65
        vertico-count 47)
  (add-hook! 'minibuffer-exit-hook
    (defun vertico-posframe-height-restore-h ()
      (setq vertico-posframe-height nil
            vertico-count 15))))

;;;###autoload
(defun restore-vertico-posframe-state-h ()
  (when (not (eq vertico-posframe-mode
                 vertico-posframe-global))
    (run-at-time
     "0 sec" nil
     (fn! ()
          (when vertico-posframe-global
            (vertico-posframe-mode +1)
            (setq vertico-posframe-global nil))))))

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
