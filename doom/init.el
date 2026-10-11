;;; init.el -*- lexical-binding: t; -*-

;; Stock Doom with direnv and C/C++ LSP. Rebuild Nix after module or package changes.
(doom! :completion
       (corfu +orderless)
       vertico

       :ui
       doom
       dashboard
       hl-todo
       modeline
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       vi-tilde-fringe
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       snippets
       (whitespace +guess +trim)

       :emacs
       dired
       electric
       tramp
       undo
       vc

       :checkers
       syntax

       :tools
       direnv
       (eval +overlay)
       lookup
       (lsp +eglot)
       magit

       :lang
       (cc +lsp)
       emacs-lisp
       markdown
       org
       sh

       :config
       (default +bindings +smartparens))
