;;; custom/web-browsing/config.el -*- lexical-binding: t; -*-

(use-package! eww
  :commands (eww +eww-open-in-other-window)
  :config
  (setq shr-use-fonts nil
        shr-max-image-proportion 0.5
        eww-browse-url-new-window-is-tab nil)

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
          :desc "copy for Org" "o" #'org-eww-copy-for-org-mode))))

(after! xwidget
  (map! :localleader :map xwidget-webkit-mode-map "x" #'kill-current-buffer))

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

  (set-popup-rule! "^\\*elfeed-entry"
                   :size 0.75 :actions '(display-buffer-below-selected)
                   :select t :quit nil :ttl t)

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

  (map! :map elfeed-search-mode-map
        :n "q" #'elfeed-kill-buffer
        "C-c C-o" #'elfeed-search-browse-url
        :n "r" #'elfeed-search-untag-all-unread
        (:localleader
         "u" #'elfeed-update))

  (map! :map elfeed-show-mode-map
        :n "yy" #'+rss/copy-link)

  ;; (evil-define-key 'normal elfeed-search-mode-map
  ;;   "q" #'elfeed-kill-buffer
  ;;   "r" #'elfeed-search-update--force
  ;;   (kbd "M-RET") #'elfeed-search-browse-url)

  (advice-add  'elfeed-show-entry :after #'+process-elfeed-entry)

  (defadvice! elfeed-search-untag-all-a (_)
    :after #'elfeed-search-untag-all
    (when-let ((nxt-entry (car (elfeed-search-selected))))
      (unless (member 'unread (elfeed-entry-tags nxt-entry))
        (forward-line -2)))))

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
