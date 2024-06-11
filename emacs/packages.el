;;; package --- use-packages
;;; Commentary:
;;; Code:

(use-package seq :ensure t)

(use-package ripgrep :ensure t)

(use-package transient :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (with-eval-after-load 'evil
	(with-eval-after-load 'elpaca-ui (evil-make-intercept-map elpaca-ui-mode-map))
	(with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map))))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package cmake-mode
  :ensure t
  :hook ((cmake-mode . lsp))
  :config (setq cmake-tab-width 4))

(use-package elixir-ts-mode
  :ensure t
  :hook ((elixir-ts-mode . lsp)))

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . lsp)))

(use-package racket-mode
  :ensure t
  :hook ((racket-mode . lsp)))

(use-package rust-mode
  :ensure t
  :init (setq rust-mode-treesitter-derive t)
  :config (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  :hook ((rust-mode . lsp)))

(use-package go-mode
  :ensure t
  :hook
  ((go-mode . lsp)
   (before-save . lsp-organize-imports)))

(use-package odin-mode
  :ensure (:host github :repo "Sampie159/odin-mode")
  :hook ((odin-mode . lsp)))

(use-package svelte-mode
  :ensure t
  :hook ((svelte-mode . lsp)))

(use-package typescript-mode
  :ensure t
  :hook ((typescript-mode . lsp)))

(use-package tuareg
  :ensure t
  :config
  (setq tuareg-match-patterns-aligned t)
  :hook ((tuareg-mode . lsp)))

(use-package zig-mode
  :ensure t
  :hook ((zig-mode . lsp)))

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit t
	which-key-separator " -> "))

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :ensure t
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :ensure t
  :after ivy
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
			       'ivy-rich-switch-buffer-transformer))

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-moe
  :config (eshell-syntax-highlighting-global-mode +1))

(use-package vterm
  :ensure t
  :config
  (setq shell-file-name "/bin/bash"
	vterm-max-scrollback 5000))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil
	vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
		   (let ((buffer (get-buffer buffer-or-name)))
		     (with-current-buffer buffer
		       (or (equal major-mode 'vterm-mode)
			   (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
		 (display-buffer-reuse-window display-buffer-at-bottom)
		 (reusable-frames . visible)
		 (window-height . 0.3))))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook
  ((c-mode . lsp)
   (c++-mode . lsp)
   (csharp-mode . lsp)
   (fortran-mode . lsp)
   (f90-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-enable-on-type-formatting nil)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "~/.local/bin/ols")
		    :major-modes '(odin-mode)
		    :server-id 'ols
		    :multi-root t))
  :commands (lsp))

(use-package lsp-ui
  :ensure t
  :config (setq lsp-ui-doc-show-with-cursor t)
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	'(("TODO" . "#E6E600")
	  ("DEBUG" . "#A020F0")
	  ("FIXME" . "#FF0000")
	  ("NOTE" . "#FF4500")
	  ("DEPRECATED" . "#1E90FF")))
  (global-hl-todo-mode t))

(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mov" . "mpv")
                                ("webm" . "mpv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package ob-rust :ensure t)

(use-package edit-indirect :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/projects/" "~/playgrounds/" "~/faculdade/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package naysayer-theme :ensure t)

(use-package kanagawa-theme
  :ensure t
  :config (load-theme 'kanagawa))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "C-i") #'company-complete-selection)))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"
	slime-contribs '(slime-fancy slime-repl slime-scratch slime-trace-dialog slime-cl-indent slime-company)))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
		slime-company-after-completion 'slime-company-just-one-space))

(use-package lsp-haskell :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode t))

(use-package evil-goggles
  :ensure t
  :config
  (setq evil-goggles-pulse t)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;;; packages.el ends here
