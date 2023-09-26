;;; custom/completion/autoload/embark.el -*- lexical-binding: t; -*-

;;;###autoload
(eval-when-compile
  (defmacro embark-ace-action (fn)
    `(defun ,(intern (concat "embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

;;;###autoload
(defmacro embark-split-action (fn split-type)
  `(defun ,(intern (concat "embark-"
                           (symbol-name fn)
                           "-"
                           (symbol-name split-type))) ()
     (interactive)
     (funcall #',split-type)
     (call-interactively #',fn)))


;;;###autoload
(defun avy-action-embark (pt)
  ;; borrowed from
  ;; https://karthinks.com/software/avy-can-do-anything/#avy-plus-embark-any-action-anywhere
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun +edebug-instrument-symbol (symbol)
  (interactive "sSymbol: ")
  (edebug-instrument-function (intern symbol)))

;;;###autoload
(defun +embark-collect-outline-cycle (&optional arg)
  (interactive "P")
  (if arg (outline-cycle-buffer)
    (outline-cycle))
  (evil-beginning-of-line))

;;;###autoload
(defun +search-rfc-number-online (&optional rfc-num)
  "Search for RFC of RFC-NUM."
  (interactive)
  (browse-url
   (format
    "https://www.rfc-editor.org/search/rfc_search_detail.php?rfc=%s"
    rfc-num)))

;;;###autoload
(defun +browse-rfc-number-at-point ()
  "Reads RFC number at point."
  (interactive)
  (if-let* ((rfc-pattern "\\b[rR][fF][cC][- ]?[0-9]+\\b")
            (bounds (org-in-regexp rfc-pattern 1))
            (rfc-num (string-to-number
                      (replace-regexp-in-string
                       "[^0-9]" ""
                       (buffer-substring-no-properties
                        (car bounds)
                        (cdr bounds))))))
      (if (featurep 'rfc-mode)
          (switch-to-buffer-other-window
           (rfc-mode--document-buffer rfc-num))
        (+search-rfc-number-online rfc-num))
    (if (featurep 'rfc-mode)
        (rfc-mode-browse)
      (+search-rfc-number-online))))

;;;###autoload
(defun +embark-project-search (target)
  (+vertico-file-search :query target))

;;;###autoload
(defun embark-open-externally+ (file)
  "Extended version of `embark-open-externally'."
  (interactive "fOpen: ")
  (pcase (file-name-extension file)
    ("mp3" (dired-file-to-mplayer file))
    ("m4a" (dired-file-to-mplayer file))
    (_ (embark-open-externally file))))
