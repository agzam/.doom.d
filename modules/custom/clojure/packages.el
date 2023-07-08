;; -*- no-byte-compile: t; -*-
;;; custom/clojure/packages.el

(package! clojure-mode)
(package! cider)
(package! clj-refactor)
(package! clojars)
(package! fennel-mode)

(package! neil :recipe (:host github :repo "babashka/neil" :files ("*.el")))

(package! logivew :recipe (:host github :repo "doublep/logview"))
(package! datetime :recipe (:host github :repo "doublep/datetime"))
