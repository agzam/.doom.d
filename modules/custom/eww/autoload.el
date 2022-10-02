;;; custom/eww/autoload.el -*- lexical-binding: t; -*-

;; borrowed from:
;; https://gitlab.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-eww.el#L584

;;;###autoload
(defun eww-open-in-other-window ()
  "Use `eww-open-in-new-buffer' in another window."
  (interactive)
  (other-window-prefix)       ; For emacs28 -- it's a hack, but why not?
  (eww-open-in-new-buffer))

;;;###autoload
(defun eww-copy-current-url+ ()
  (let ((url (eww-current-url)))
    (kill-new url)
    (message url)))

;;;###autoload
(defun +eww-increase-font-size ()
  (interactive)
  (if shr-use-fonts
      (let* ((cur (face-attribute 'shr-text :height nil))
             (cur (if (floatp cur) cur 1.0)))
        (set-face-attribute 'shr-text nil :height (+ cur 0.1)))
    (text-scale-increase 0.5)))

;;;###autoload
(defun +eww-decrease-font-size ()
  (interactive)
  (if shr-use-fonts
      (let* ((cur (face-attribute 'shr-text :height nil))
             (cur (if (floatp cur) cur 1.0)))
        (set-face-attribute 'shr-text nil :height (- cur 0.1)))
    (text-scale-decrease 0.5)))

(require 'transient)
;;;###autoload
(transient-define-prefix eww-zoom-transient ()
  "EWW"
  ["Fonts"
   [("j" "decrease" +eww-decrease-font-size :transient t)
    ("k" "increase" +eww-increase-font-size :transient t)]])
