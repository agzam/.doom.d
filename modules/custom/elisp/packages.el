;; -*- no-byte-compile: t; -*-
;;; custom/elisp/packages.el

(package! a)
(package! paradox)
(package! let-plist :recipe (:local-repo "let-plist" :files ("*.el")))
(package! evilify-edebug :recipe (:local-repo "evilify-edebug" :files ("*.el")))

(unpin! buttercup)
