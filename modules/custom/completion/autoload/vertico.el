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

;;;###autoload
(defun +vertico-orderless-dispatch (pattern _index _total)
  "Like `orderless-affix-dispatch', but allows affixes to be escaped."
  (let ((len (length pattern))
        (alist orderless-affix-dispatch-alist))
    (when (> len 0)
      (cond
       ;; Ignore single dispatcher character
       ((and (= len 1) (alist-get (aref pattern 0) alist)) #'ignore)
       ;; Prefix
       ((when-let ((style (alist-get (aref pattern 0) alist))
                   ((not (char-equal (aref pattern (max (1- len) 1)) ?\\))))
          (cons style (substring pattern 1))))
       ;; Suffix
       ((when-let ((style (alist-get (aref pattern (1- len)) alist))
                   ((not (char-equal (aref pattern (max 0 (- len 2))) ?\\))))
          (cons style (substring pattern 0 -1))))))))

;;;###autoload
(defun +vertico-orderless-disambiguation-dispatch (pattern _index _total)
  "Ensure $ works with Consult commands, which add disambiguation suffixes."
  (let ((len (length pattern)))
    (when (and (> len 0)
               (char-equal (aref pattern (1- len)) ?$))
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))))


;;;###autoload
(defun +vertico/embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))
