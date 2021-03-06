;;; custom/pdf/config.el -*- lexical-binding: t; -*-

(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (after! pdf-annot
    (defun +pdf-cleanup-windows-h ()
      "Kill left-over annotation buffers when the document is killed."
      (when (buffer-live-p pdf-annot-list-document-buffer)
        (pdf-info-close pdf-annot-list-document-buffer))
      (when (buffer-live-p pdf-annot-list-buffer)
        (kill-buffer pdf-annot-list-buffer))
      (let ((contents-buffer (get-buffer "*Contents*")))
        (when (and contents-buffer (buffer-live-p contents-buffer))
          (kill-buffer contents-buffer))))
    (add-hook! 'pdf-view-mode-hook
      (add-hook 'kill-buffer-hook #'+pdf-cleanup-windows-h nil t)))

  :config
  (defadvice! +pdf--install-epdfinfo-a (fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    :around #'pdf-view-mode
    (if (file-executable-p pdf-info-epdfinfo-program)
        (apply fn args)
      ;; If we remain in pdf-view-mode, it'll spit out cryptic errors. This
      ;; graceful failure is better UX.
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it")))

  (pdf-tools-install-noverify)

  ;; For consistency with other special modes
  (map! :map pdf-view-mode-map
        :gn "q" #'kill-current-buffer
        :n "J" #'pdf-view-next-page
        :n "K" #'pdf-view-previous-page
        :n "gg" #'pdf-view-first-page
        :n "G"  #'pdf-view-last-page
        :n "[" #'pdf-history-backward
        :n "]" #'pdf-history-forward
        :n "o" #'pdf-outline
        :localleader
        "n" #'pdf-view-midnight-minor-mode
        (:prefix ("s" . "slice/scroll")
         "b" #'pdf-view-set-slice-from-bounding-box
         "m" #'pdf-view-set-slice-using-mouse
         "r" #'pdf-view-reset-slice
         "s" #'pdf-view-roll-minor-mode)
        (:prefix ("f" . "fit")
         "h" #'pdf-view-fit-height-to-window
         "p" #'pdf-view-fit-page-to-window
         "w" #'pdf-view-fit-width-to-window)
        (:prefix ("z" . "zoom")
         "k" #'pdf-view-enlarge
         "j" #'pdf-view-shrink
         "0" #'pdf-view-scale-reset))

  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  ;; The mode-line does serve any useful purpose is annotation windows
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)

  ;; HACK Fix #1107: flickering pdfs when evil-mode is enabled
  (setq-hook! 'pdf-view-mode-hook evil-normal-state-cursor (list nil))

  ;; Silence "File *.pdf is large (X MiB), really open?" prompts for pdfs
  (defadvice! +pdf-suppress-large-file-prompts-a (fn size op-type filename &optional offer-raw)
    :around #'abort-if-file-too-large
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw))))

(use-package! saveplace-pdf-view
  :after pdf-view)

(after! pdf-view
  (defadvice! pdf-view-midnight-minor-mode-a (fn &rest args)
    :around #'pdf-view-midnight-minor-mode
    (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                     ,(face-attribute 'default :background)))
    (funcall fn args)))
