;; -*- no-byte-compile: t; -*-

(package! winum)
(package! info+)

(package! expreg)

(unpin! wgrep)

(package! ibuffer-sidebar)

(package! which-key-posframe)

(package! visual-fill-column
  ;; original is on unreliable codeberg.org
  :recipe (:host github :repo "emacsmirror/visual-fill-column"))

(package! undo-fu
  ;; original is on unreliable codeberg.org
  :recipe (:host github :repo "emacsmirror/undo-fu"))

(unpin! undo-fu)

(package! undo-fu-session
  ;; original is on unreliable codeberg.org
  :recipe (:host github :repo "emacsmirror/undo-fu-session"))

(unpin! undo-fu-session)
