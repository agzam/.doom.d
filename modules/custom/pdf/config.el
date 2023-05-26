;;; custom/pdf/config.el -*- lexical-binding: t; -*-

(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
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

  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  ;; The mode-line does serve any useful purpose is annotation windows
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)

  ;; HACK Fix #1107: flickering pdfs when evil-mode is enabled
  ;; (setq-hook! 'pdf-view-mode-hook evil-normal-state-cursor (list nil))

  ;; Silence "File *.pdf is large (X MiB), really open?" prompts for pdfs
  ;; (defadvice! +pdf-suppress-large-file-prompts-a (fn size op-type filename &optional offer-raw)
  ;;   :around #'abort-if-file-too-large
  ;;   (unless (string-match-p "\\.pdf\\'" filename)
  ;;     (funcall fn size op-type filename offer-raw)))

  (map! :map pdf-view-mode-map
        :gn "q" #'kill-current-buffer
        :nm "J" #'pdf-view-next-page
        :nm "K" #'pdf-view-previous-page
        :n "gg" #'pdf-view-first-page
        :n "G"  #'pdf-view-last-page
        :nm "[" #'pdf-history-backward
        :nm "]" #'pdf-history-forward
        :nm "o" #'pdf-outline
        :nm "C-e" #'evil-collection-pdf-view-next-line-or-next-page
        :nm "C-y" #'evil-collection-pdf-view-previous-line-or-previous-page
        :nm "zk" #'pdf-view-enlarge
        :nm "zj" #'pdf-view-shrink
        :localleader
        "t" #'pdf-view-themed-minor-mode
        "," #'pdf-view-current-progress
        (:prefix ("s" . "slice/scroll")
                 "a" #'pdf-view-auto-slice-minor-mode
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
                 "0" #'pdf-view-scale-reset)
        (:prefix ("n" . "noter")
                 "N" #'org-noter
                 "n" #'org-noter-sync-current-page-or-chapter
                 "i" #'org-noter-insert-note)))

(use-package! saveplace-pdf-view
  :after pdf-view)

(after! pdf-view
  (defadvice! pdf-view-midnight-minor-mode-a (fn &rest args)
    :around #'pdf-view-midnight-minor-mode
    (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                     ,(face-attribute 'default :background)))
    (funcall fn args)))


(use-package! org-noter
  :defer t
  :config
  (setq
   org-noter-notes-search-path (list org-directory)
   org-noter-auto-save-last-location nil
   org-noter-separate-notes-from-heading t
   org-noter-always-create-frame nil
   org-noter-insert-note-no-questions nil
   org-noter-disable-narrowing t
   org-noter-notes-window-behavior '(only-prev)
   org-noter-kill-frame-at-session-end nil)

                                        ; (defadvice! recenter-on-org-noter-sync-current-page-or-chapter-a ()
                                        ;   :after #'org-noter-sync-current-page-or-chapter
                                        ;   (recenter))
  )

