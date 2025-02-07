;;; custom/web-browsing/config.el -*- lexical-binding: t; -*-

(use-package! eww
  :commands (eww +eww-open-in-other-window)
  :config
  (setq shr-use-fonts nil
        shr-max-image-proportion 0.5
        eww-browse-url-new-window-is-tab nil
        shr-max-width 80
        shr-put-image-function #'shr-put-image*)

  (add-hook! 'eww-after-render-hook #'eww--rename-buffer)
  (defadvice! eww-rename-buffer-a ()
    :after #'eww-back-url
    :after #'eww-forward-url
    (eww--rename-buffer))

  (map! :map eww-mode-map
        "C-c C-o" #'eww-browse-with-external-browser
        :n "C-j" (cmd! () (pixel-scroll-precision-scroll-down 50))
        :n "C-k" (cmd! () (pixel-scroll-precision-scroll-up 50))
        :n "j" #'evil-next-visual-line
        :n "k" #'evil-previous-visual-line
        :ni "C-<return>" #'+eww-open-in-other-window
        :n "yy" #'+eww-copy-current-url
        :n "zk" #'+eww-increase-font-size
        :n "zj" #'+eww-decrease-font-size
        :n "q" #'kill-buffer-and-window
        [remap imenu] #'+eww-jump-to-url-on-page

        (:localleader
         :desc "zoom" "z" #'eww-zoom-transient
         :desc "external browser" "e" #'eww-browse-with-external-browser
         :desc "buffers" "b" #'eww-switch-to-buffer
         :desc "reload" "r" #'eww-reload
         (:prefix ("t" . "toggle")
          :desc "readable" "r" #'eww-readable
          :desc "colors" "c" #'eww-toggle-colors
          :desc "fonts" "f" #'eww-toggle-fonts
          :desc "images" "i" #'eww-toggle-images)

         (:prefix ("y" . "copy")
          :desc "copy url" "y" #'+eww-copy-current-url
          :desc "copy for Org" "o" #'org-eww-copy-for-org-mode)))

  ;; (advice-add #'eww-display-html :around #'eww-make-readable-a)
  )

(after! xwidget
  (map!
   :map xwidget-webkit-mode-map
   :n "zk" #'xwidget-webkit-zoom-in
   :n "zj" #'xwidget-webkit-zoom-out
   :localleader
   "x" #'kill-current-buffer))

(use-package! browser-hist
  :init
  (require 'embark)
  (setq browser-hist-default-browser 'brave)
  :commands (browser-hist-search))

(use-package! elfeed
  :commands elfeed
  :init
  (setq elfeed-db-directory (concat doom-local-dir "elfeed/db/")
        elfeed-enclosure-default-dir (concat doom-local-dir "elfeed/enclosures/"))
  :config
  (setq elfeed-search-filter "@2-months-old "
        elfeed-show-entry-switch #'switch-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-search-title-max-width 80
        shr-max-image-proportion 0.8)

  (make-directory elfeed-db-directory t)

  ;; Ensure elfeed buffers are treated as real
  (add-hook! 'doom-real-buffer-functions
    (defun +rss-buffer-p (buf)
      (string-match-p "^\\*elfeed" (buffer-name buf))))

  ;; Enhance readability of a post
  (add-hook 'elfeed-show-mode-hook #'+rss-elfeed-wrap-h)
  (add-hook! 'elfeed-search-mode-hook
    (add-hook 'kill-buffer-hook #'+rss-cleanup-h nil 'local))

  ;; Large images are annoying to scroll through, because scrolling follows the
  ;; cursor, so we force shr to insert images in slices.
  (setq-hook! 'elfeed-show-mode-hook
    shr-put-image-function #'+rss-put-sliced-image-fn
    shr-external-rendering-functions '((img . +rss-render-image-tag-without-underline-fn)))

  ;; Keybindings
  (after! elfeed-show
    (define-key! elfeed-show-mode-map
      [remap next-buffer]     #'+rss/next
      [remap previous-buffer] #'+rss/previous))

  (add-hook! 'elfeed-search-mode-hook
    (defun elfeed-set-search-keys ()
      (map! :map elfeed-search-mode-map
            :n "q" #'elfeed-kill-buffer
            "C-c C-o" #'elfeed-search-browse-url
            :n "r" #'elfeed-search-untag-all-unread
            (:localleader
             "u" #'elfeed-update
             "p" #'elfeed-update))))

  (add-hook! 'elfeed-show-mode-hook
    (defun elfeed-set-show-keys ()
      (map! :map elfeed-show-mode-map
            :n "yy" #'+rss/copy-link)))

  ;; (evil-define-key 'normal elfeed-search-mode-map
  ;;   "q" #'elfeed-kill-buffer
  ;;   "r" #'elfeed-search-update--force
  ;;   (kbd "M-RET") #'elfeed-search-browse-url)

  (advice-add  'elfeed-show-entry :after #'+process-elfeed-entry))

(use-package! elfeed-org
  :after elfeed
  :preface
  (setq rmh-elfeed-org-files (list (concat doom-user-dir "elfeed.org")))
  :config
  (elfeed-org)
  (defadvice! +rss-skip-missing-org-files-a (&rest _)
    :before '(elfeed rmh-elfeed-org-mark-feed-ignore elfeed-org-export-opml)
    (unless (file-name-absolute-p (car rmh-elfeed-org-files))
      (let* ((default-directory org-directory)
             (files (mapcar #'expand-file-name rmh-elfeed-org-files)))
        (dolist (file (cl-remove-if #'file-exists-p files))
          (message "elfeed-org: ignoring %S because it can't be read" file))
        (setq rmh-elfeed-org-files (cl-remove-if-not #'file-exists-p files))))))

(use-package! elfeed-tube
  :after elfeed
  :config
  (elfeed-tube-setup)

  (map! :map elfeed-show-mode-map
        (:localleader
         "f" #'elfeed-tube-mpv-follow-mode))

  ;; (map! :map elfeed-show-mode-map
  ;;       "F" #'elfeed-tube-fetch
  ;;       [remap save-buffer] #'elfeed-tube-save
  ;;       :localleader
  ;;       :desc "Youtube fetch" "f" #'elfeed-tube-fetch
  ;;       :map elfeed-search-mode-map
  ;;       "F" #'elfeed-tube-fetch
  ;;       [remap save-buffer] #'elfeed-tube-save
  ;;       :localleader
  ;;       :desc "Youtube fetch" "f" #'elfeed-tube-fetch)

  )

(use-package! yeetube
  :config
  (setq yeetube-play-function #'mpv-open+
        yeetube-results-limit 100)
  (map! :map yeetube-mode-map
        [remap evil-ret] #'yeetube-play
        "C-c C-o" #'yeetube-browse-url
        :localleader
        "s" #'yeetube-search))

(after! mpv
  (setq mpv-volume-step 1.1))

(use-package! elfeed-tube-mpv
  :defer t
  :config
  (add-hook! 'elfeed-show-mode-hook
    (defun elfeed-set-tube-keys ()
      (map! :map elfeed-show-mode-map
            :localleader
            :desc "mpv" "," #'mpv-transient
            :desc "mpv play" "p" #'elfeed-tube-mpv)))

  (add-to-list
   'elfeed-tube-mpv-options
   "--script=~/.config/mpv/scripts/post-load.lua"))


(use-package! rfc-mode)

(use-package! subed
  :defer t
  :config
  (add-hook! 'subed-mode-hook
             #'subed-enable-pause-while-typing
             #'subed-enable-sync-player-to-point
             #'subed-enable-sync-point-to-player)
  (map! :map subed-mode-map
        :localleader
        (:prefix ("t" . "toggle")
                 "t" #'subed-toggle-srt-metadata)
        "p" #'subed-mpv-play-from-file+))

(use-package! hnreader
  :defer t
  :config)

(use-package! consult-hn
  :commands (consult-hn)
  :defer t
  :config)

(use-package! reddigg
  :defer t
  :config
  (setq reddigg-subs '(emacs clojure))

  (defadvice! reddig-expand-all-a ()
    :after #'reddigg--ensure-modes
    (org-fold-show-all)
    (goto-char (point-min))
    (org-next-visible-heading 1)))

