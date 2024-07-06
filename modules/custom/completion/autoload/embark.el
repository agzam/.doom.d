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
  (require 'org)
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

;;;###autoload
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

;;;###autoload
(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

;;;###autoload
(defun embark-preview+ ()
  "My custom embark preview function."
  (interactive)
  (when-let* ((target (car (embark--targets)))
              (type (plist-get target :type)))
    (cond
     ((and (member type '(url consult-web))
           (string-match-p
            ;; only match PRs/Issues or individual files
            "https://github\\.com/\\([^/]+/[^/]+/\\)\\(pull\\|issues\\|blob\\)[^#\n]+"
            (plist-get target :target)))
      (cl-labels ((forge-visit-topic-url*
                    (url &rest _)
                    (forge-visit-topic-via-url url)))
        (embark--act #'forge-visit-topic-url* target nil)))

     ((member type '(url consult-web))
      (cl-labels ((eww-browse-url*
                    (url &rest _)
                    (eww-browse-url url)))
        (embark--act #'eww-browse-url* target nil)))

     ((fboundp 'embark-dwim)
      (save-selected-window
        (let (embark-quit-after-action)
          (embark-dwim)))))))
