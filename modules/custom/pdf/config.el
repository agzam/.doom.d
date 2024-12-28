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
        :n "gg" #'pdf-evil-goto-first-line
        :n "G"  #'pdf-evil-goto-last-line
        :nm "[" #'pdf-history-backward
        :nm "]" #'pdf-history-forward
        :nm "o" #'pdf-outline
        :nm "C-e" #'pdf-view-scroll-up-or-next-page
        :nm "C-y" #'pdf-view-scroll-down-or-previous-page
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
                 "s" #'pdf-view-roll-minor-mode
                 "c" #'pdf-toggle-continuous-scroll)
        (:prefix ("f" . "fit")
                 "h" #'pdf-view-fit-height-to-window
                 "p" #'pdf-view-fit-page-to-window
                 "w" #'pdf-view-fit-width-to-window)
        (:prefix ("z" . "zoom")
                 "k" #'pdf-view-enlarge
                 "j" #'pdf-view-shrink
                 "0" #'pdf-view-scale-reset)
        "n" #'org-noter-transient))

(use-package! saveplace-pdf-view
  :after pdf-view)

(after! pdf-view
  (defadvice! pdf-view-midnight-minor-mode-a (fn &rest args)
    "Toggling midnight-mode uses current theme colors."
    :around #'pdf-view-midnight-minor-mode
    (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                     ,(face-attribute 'default :background)))
    (funcall fn args))

  (defadvice! pdf-view-next-page-at-top-of-the-page-a (&optional N)
    "Always start at the top of the page."
    :after #'pdf-view-next-page
    :after #'pdf-view-next-page-command
    (image-scroll-down)))


(use-package! org-noter
  :after (org pdf-tools)
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

  (defadvice! org-noter--setup-windows-ignore-a (_ session)
    "Cease org-noter's windows and frame shenanigans."
    :around #'org-noter--setup-windows
    (when (org-noter--valid-session session)
      (with-selected-frame (org-noter--session-frame session)
        (let* ((doc-buffer (org-noter--session-doc-buffer session))
               (notes-buffer (org-noter--session-notes-buffer session)))
          (switch-to-buffer-other-window doc-buffer)
          (with-current-buffer notes-buffer
            (unless org-noter-disable-narrowing
              (org-noter--narrow-to-root (org-noter--parse-root session)))
            (setq notes-window (org-noter--get-notes-window 'start))
            (org-noter--set-notes-scroll notes-window))))))

  (defadvice! org-noter--set-notes-scroll-ignore-a (&rest args)
    :override #'org-noter--set-notes-scroll)

  (defadvice! org-noter--create-session-a (orig-fn &rest args)
    :around #'org-noter--create-session
    (cl-letf (((symbol-function
                #'org-noter--set-notes-scroll)
               #'ignore))
      (apply orig-fn args))))
