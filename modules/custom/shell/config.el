;;; custom/shell/config.el -*- lexical-binding: t; -*-
(defun +insert-current-filename ()
  (interactive)
  (when (eq major-mode 'minibuffer-mode)
    (let ((fname (with-current-buffer
                     (window-buffer (minibuffer-selected-window))
                   (pcase major-mode
                     ('dired-mode
                      (dired-get-filename))
                     (_ buffer-file-name)))))
      (insert fname))))

(map! :map minibuffer-local-map "C-c C-i" #'+insert-current-filename)

(after! shell
  ;; Something messes up blue color in terminal. Usual way of setting it up in the theme,
  ;; not working for some reason. This is a hacky workaround
  (add-hook! '(shell-mode-hook doom-load-theme-hook)
    (defun set-shell-colors ()
      ;; I don't know how to find out if the current theme is dark or light
      (when (custom-theme-enabled-p 'ag-themes-base16-ocean)
        (set-face-attribute 'ansi-color-blue nil :foreground "#00bfff"))))

  (map! :map shell-mode-map
        "C-j" nil
        "C-c C-l" #'comint-clear-buffer
        :localleader
        "c" #'comint-clear-buffer))

(after! eshell
  (setq eshell-aliases-file (concat doom-emacs-dir ".local/etc/eshell/aliases"))
  (add-hook! 'eshell-mode-hook
    (defun set-eshell-keys-h ()
      (map! :map eshell-mode-map
            :desc "clear" "C-c C-l" #'eshell-clear+
            :desc "detach" "C-<return>" #'eshell-send-detached-input
            :desc "kitty detach" "s-<return>" #'eshell-send-detached-input-to-kitty
            :i "C-u" nil
            (:localleader
             :desc "clear" "c" #'eshell-clear+
             "b" #'eshell-insert-buffer-name))
      (map! :map eshell-hist-mode-map
            :desc "clear" "C-c C-l" #'eshell-clear+
            :desc "history" "M-r" #'consult-history
            :desc "output>buf" "C-c C-h" #'eshell-export-output+
            (:unless (featurep 'eshell-atuin)
              :desc "history" "M-r" #'consult-history)
            (:when (featurep 'eshell-atuin)
              :desc "history" "M-r" #'eshell-atuin-history))))

  (add-hook! '(comint-mode-hook
               eshell-mode-hook)
             #'update-hyprland-env-h)

  (cl-defmethod eshell-output-object-to-target :around (_obj (target marker))
    ;; immediately open the redirected buffer
    (let ((base (cl-call-next-method)))
      (when (buffer-live-p (marker-buffer target))
        (with-current-buffer (marker-buffer target)
          (ansi-color-apply-on-region (point-min) (point-max))
          (display-buffer (current-buffer))))
      base))

  ;; fullscreen apps
  (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual)
  (dolist (cmd '(ncdu btop htop k9s pueue procs lazydocker))
    (add-to-list 'eshell-visual-commands (symbol-name cmd))))

(use-package! shell-pop
  :defer t
  :init
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (interactive) (eshell))))
  :config
  (setq shell-pop-window-position "bottom"))


(use-package! vimrc-mode
  :defer t
  :mode "\\.vim\\(rc\\)?\\'")

(use-package! eat
  :defer t
  ;; :hook ((eshell-load . eat-eshell-mode))
  :config
  ;; macOS ncurses can't read the 32-bit terminfo entries bundled with eat.
  ;; We compile them with the system tic (see below) and point eat at ~/.terminfo.
  (when (eq system-type 'darwin)
    (let ((user-terminfo (expand-file-name "~/.terminfo")))
      (unless (file-exists-p (expand-file-name "65/eat-truecolor" user-terminfo))
        (let ((ti-src (expand-file-name "eat.ti"
                        (file-name-directory (locate-library "eat")))))
          (when (file-exists-p ti-src)
            (make-directory user-terminfo t)
            (call-process "tic" nil nil nil "-o" user-terminfo ti-src))))
      (setq eat-term-terminfo-directory user-terminfo)))

  ;; Disable modes that add per-keystroke overhead in a terminal buffer.
  (add-hook! 'eat-mode-hook
    (defun +eat-disable-noisy-modes-h ()
      (flycheck-mode -1)
      (smartparens-mode -1)
      (jinx-mode -1)
      (font-lock-mode -1)
      (yas-minor-mode -1)
      (corfu-mode -1)))

  ;; In char mode, evil must get out of the way entirely - every keystroke
  ;; goes to the terminal (including ESC). This is required for TUI apps
  ;; like claude, htop, etc. The only escape hatch is C-M-m (back to semi-char).
  (evil-define-key* '(insert normal visual) eat-char-mode-map
    [escape] #'eat-self-input)
  (evil-set-initial-state 'eat-char-mode 'emacs)

  ;; Protect point/mark during eat terminal resizes.
  ;; Any popup (transient, which-key, etc.) that resizes the eat window
  ;; triggers eat-term-resize -> eat--t-break-long-line that reflows
  ;; terminal text and moves point as a side effect.
  (defadvice! +eat-preserve-point-on-resize-a (ofn process windows)
    :around #'eat--adjust-process-window-size
    (let ((buf (process-buffer process)))
      (if (and buf (buffer-live-p buf)
               (buffer-local-value 'eat-terminal buf))
          (with-current-buffer buf
            (let ((pt (point))
                  (mk (mark t)))
              (funcall ofn process windows)
              (goto-char (min pt (point-max)))
              (when mk (set-mark (min mk (point-max))))))
        (funcall ofn process windows))))

  ;; Suppress eat's scroll-sync while in visual state - otherwise
  ;; eat--synchronize-scroll snaps point back to the terminal cursor
  ;; every time output arrives, destroying the selection.
  (add-hook! 'evil-visual-state-entry-hook
    (defun +eat-suppress-scroll-sync-h ()
      (when (derived-mode-p 'eat-mode)
        (setq-local eat--synchronize-scroll-function #'ignore))))

  (add-hook! 'evil-visual-state-exit-hook
    (defun +eat-restore-scroll-sync-h ()
      (when (derived-mode-p 'eat-mode)
        (setq-local eat--synchronize-scroll-function #'eat--synchronize-scroll))))

  ;; When switching between eat input modes, sync evil state accordingly.
  (add-hook! 'eat-char-mode-hook
    (defun +eat-char-mode-sync-evil-h ()
      (if eat-char-mode
          (evil-emacs-state)
        (evil-insert-state))))

  (map! :map eat-mode-map
   :i "C-j" #'eat-self-input
   ;; Quick toggle into char mode for TUI apps (claude, etc.)
   :i "C-c C-d" #'eat-char-mode
   "s-v" #'eat-yank-from-kill-ring
   ;; Also available from normal state
   :n "C-c C-d" #'eat-char-mode))

(use-package! eshell-atuin
  :when (executable-find "atuin")
  :after eshell
  :init (eshell-atuin-mode)
  :config
  (setopt
   eshell-atuin-search-fields '(time duration command directory relativetime)
   eshell-atuin-history-format "%-70c %>10r %-40i "
   eshell-atuin-filter-mode 'global
   eshell-atuin-search-options nil)

  (defadvice! eshell-atuin-history-fix-sorting-a (ofn &optional arg)
    :around #'eshell-atuin-history
    (let* ((vertico-sort-function nil))
      (funcall ofn arg))))

(use-package! yuck-mode
  :defer t)

(use-package! mise
  :defer t
  ;; :hook (doom-init-ui . global-mise-mode)
  :when (executable-find "mise"))

(defun on-mxp-buffer-update-h (buffer-name beg end)
  (with-current-buffer buffer-name
    (ansi-color-apply-on-region beg end)))

(add-hook 'mxp-buffer-update-hook #'on-mxp-buffer-update-h)
