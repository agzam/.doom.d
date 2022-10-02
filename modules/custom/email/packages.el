;; -*- no-byte-compile: t; -*-

(package! notmuch :recipe (:host github :repo "notmuch/notmuch"))

(when (modulep! +org)
  (package! org-mime))
(when (modulep! :lang org)
  (package! ol-notmuch))
(when (modulep! :completion vertico)
  (package! consult-notmuch))
