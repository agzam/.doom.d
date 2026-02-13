;; -*- no-byte-compile: t; -*-
;;; custom/writing/packages.el
(package! mw-thesaurus)
(package! sdcv)
(package! google-translate :recipe
  (:host github :repo "agzam/google-translate"
         :branch "improvements"))

(package! define-it)

(when (eq system-type 'darwin)
  ;; ln -s ~/.hammerspoon/ ~/.doom.d/modules/custom/general/spacehammer
  (package! spacehammer :recipe (:local-repo "spacehammer" :files ("*.el"))))

(package! separedit :recipe (:host github :repo "twlz0ne/separedit.el"))

;; (package! youtube-sub-extractor :recipe (:local-repo "youtube-sub-extractor.el"))
(package! youtube-sub-extractor)

(package! wiktionary-bro :recipe (:local-repo "wiktionary-bro"))

(package! jinx :recipe (:host github :repo "minad/jinx"))

;; translate-popup retired - functionality now in google-translate-posframe-ui
;; (package! translate-popup :recipe (:local-repo "translate-popup"))
