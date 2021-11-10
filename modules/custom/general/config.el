;;; common/general/config.el -*- lexical-binding: t; -*-

(use-package! embark-consult
  :config)

;; (use-package! prescient
;;   :config)

(use-package! fasd
  :config
  (map! :leader "fad" #'fasd-find-file)
  (global-fasd-mode +1))

(use-package! consult-company
  :config
  (map! :map company [remap completion-at-point] #'consult-company))

(use-package! vertico-posframe
  :config
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (vertico-posframe-mode +1)
  (setq marginalia-margin-threshold 300))

;; Add vertico extensions load path
(add-to-list 'load-path (format "%sstraight/build-%s/vertico/extensions/" (file-truename doom-local-dir) emacs-version))

(use-package! vertico-repeat
  :after vertico
  :config
  (map! :leader "rl" #'vertico-repeat)
  (add-hook! minibuffer-setup #'vertico-repeat-save))

(use-package! vertico-quick
  :after vertico
  :config
  (map! :map vertico-map "C-'" #'vertico-quick-insert))

(use-package! vertico-directory
  :after vertico
  :config
  (map! :map vertico-map "C-h" #'vertico-directory-delete-word))

(use-package! vertico-grid
  :after vertico
  :config
  (map! :map vertico-map
        "C-c g" #'vertico-grid-mode
        "M-h" #'vertico-grid-left
        "M-l" #'vertico-grid-right)
  (add-hook! minibuffer-exit
    (defun vertico-grid-mode-off ()
      (vertico-grid-mode -1))))

(use-package! vertico-buffer :after vertico)

(after! vertico
  (map! :map vertico-map
        "C-e" #'vertico-scroll-up
        "C-y" #'vertico-scroll-down
        ;; unbind universal argument
        "C-u" nil))

(after! ibuf-ext
  (setq
   ibuffer-old-time 8 ; buffer considered old after that many hours
   ibuffer-group-buffers-by 'projects
   ibuffer-expert t
   ibuffer-show-empty-filter-groups nil)

  (define-ibuffer-filter unsaved-file-buffers
      "Toggle current view to buffers whose file is unsaved."
    (:description "file is unsaved")
    (ignore qualifier)
    (and (buffer-local-value 'buffer-file-name buf)
         (buffer-modified-p buf)))

  (define-ibuffer-filter file-buffers
      "Only show buffers backed by a file."
    (:description "file buffers")
    (ignore qualifier)
    (buffer-local-value 'buffer-file-name buf))

  (map! :map ibuffer-mode-map
        :n "su" #'ibuffer-filter-by-unsaved-file-buffers
        :n "sF" #'ibuffer-filter-by-file-buffers))
