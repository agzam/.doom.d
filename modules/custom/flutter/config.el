;;; custom/flutter/config.el -*- lexical-binding: t; -*-

(use-package! dart-mode
  :hook (dart-mode . rainbow-delimiters-mode)
  :config
  (when (modulep! +lsp)
    (add-hook 'dart-mode-local-vars-hook #'lsp! 'append))
  (set-ligatures! '(dart-mode)
                  ;; Functional
                  :def "Function"
                  :lambda "() =>"
                  ;; Types
                  :null "null"
                  :true "true" :false "false"
                  :int "int" :float "double"
                  :str "String"
                  :bool "bool"
                  :list "List"
                  ;; Flow
                  :not "!"
                  :in "in"
                  :and "&&" :or "||"
                  :for "for"
                  :return "return"
                  ;; Other
                  :yield "yield"))

(use-package! flutter
  :defer t
  :init
  (map! :after dart-mode
        :map dart-mode-map
        :localleader
        (:prefix ("F" . "flutter")
                 "f" #'flutter-run
                 "q" #'flutter-quit
                 "r" #'flutter-hot-reload
                 "R" #'flutter-hot-restart)))

(use-package! lsp-dart
  :when (modulep! +lsp)
  :defer t
  :config
  (setq lsp-dart-closing-labels nil)
  (setq lsp-signature-doc-lines 0)
  (map! :map dart-mode-map
        (:localleader
         (:prefix ("T" . "test")
                  "t" #'lsp-dart-run-test-at-point
                  "a" #'lsp-dart-run-all-tests
                  "f" #'lsp-dart-run-test-file
                  "l" #'lsp-dart-run-last-test
                  "v" #'lsp-dart-visit-last-test))))
