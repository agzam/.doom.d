;;; custom/python/config.el -*- lexical-binding: t; -*-

(add-hook! (python-ts-mode
            python-mode)
           #'lsp!)
