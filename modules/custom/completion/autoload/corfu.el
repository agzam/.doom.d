;;; custom/completion/autoload/corfu.el -*- lexical-binding: t; -*-
;;; Courtesy of Takeshi Tsukamoto
;;; https://github.com/itome/.doom.d/

;;;###autoload
(defun +corfu--enable-in-minibuffer ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (memq this-command '(evil-ex
                                   evil-ex-search-forward
                                   evil-ex-search-backward))
              (setq-local corfu-echo-delay nil
                          corfu-popupinfo-delay nil)
              (corfu-mode +1))))


;;;###autoload
(defun +corfu-complete-file-at-point ()
  "Complete a file path from scratch at point"
  (interactive)
  (completion-in-region (point) (point) #'read-file-name-internal))

;;;###autoload
(defun +corfu-files ()
  "Complete using files source"
  (interactive)
  (let ((completion-at-point-functions (list #'+file-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun +corfu-dabbrev ()
  "Complete using dabbrev source"
  (interactive)
  (let ((completion-at-point-functions (list #'+dabbrev-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun +corfu-ispell ()
  "Complete using ispell source.
See `ispell-lookup-words' for more info"
  (interactive)
  (let ((completion-at-point-functions (list #'+ispell-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun +corfu-dict ()
  "Complete using dict source.
See `+dict--words' for extra words, and `+dict-file' for a wordslist source "
  (interactive)
  (let ((completion-at-point-functions (list #'+dict-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun +corfu-move-to-minibuffer ()
  (interactive)
  (let (completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

;;;###autoload
(defun +corfu-kill-frames ()
  (interactive)
  (ignore-errors
    (when vertico-posframe-mode
      (vertico-posframe-cleanup))
    (when (boundp 'corfu--frame)
      (delete-frame corfu--frame))
    (when (boundp 'corfu-popupinfo--frame)
      (delete-frame corfu-popupinfo--frame))))

;;;###autoload
(defun +corfu-insert-indexed (idx)
  (interactive)
  (let ((corfu--index idx))
    (call-interactively #'corfu-insert)))

;;;###autoload
(defun +corfu-quit-and-escape ()
  "Call `corfu-quit' and then return to Normal State."
  (interactive)
  (call-interactively 'corfu-quit)
  (evil-normal-state))
