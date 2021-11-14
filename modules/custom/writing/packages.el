;; -*- no-byte-compile: t; -*-
;;; custom/writing/packages.el
(package! mw-thesaurus)
(package! sdcv-mode :recipe (:host github :repo "gucong/emacs-sdcv"))
(package! google-translate)
(package! keytar :recipe (:host github :repo "emacs-grammarly/keytar"))
(package! lsp-grammarly :recipe (:host github :repo "emacs-grammarly/lsp-grammarly"))
(package! define-it)

(package! flyspell-correct)
(package! flyspell-lazy)

(when (eq system-type 'darwin)
  ;; ln -s ~/.hammerspoon/ ~/.doom.d/modules/custom/general/spacehammer
  (package! spacehammer :recipe (:local-repo "spacehammer" :files ("*.el"))))
