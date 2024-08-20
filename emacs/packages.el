;;; package --- use-packages
;;; Commentary:
;;; Code:

(use-package seq :ensure t)

(use-package ripgrep :ensure t)

(use-package transient :ensure t)

(use-package treesit)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package c-ts-mode
  :config
  (setq c-ts-mode-indent-offset 4))

(use-package cmake-mode
  :ensure t
  :hook ((cmake-mode . lsp))
  :config
  (setq cmake-tab-width 4))

(use-package elixir-ts-mode
  :ensure t
  :hook ((elixir-ts-mode . lsp)))

(use-package gleam-ts-mode
  :load-path "~/Downloads/gleam-mode"
  :init (add-to-list 'auto-mode-alist '("\\.gleam\\'" . gleam-ts-mode)))

(use-package go-ts-mode
  :hook
  ((go-ts-mode . lsp)
   (before-save . lsp-organize-imports))
  :init (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  :config
  (setq go-ts-mode-indent-offset 4))

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . lsp))
  :config (setq lsp-haskell-formatting-provider "stylish-haskell"))

(use-package racket-mode
  :ensure t
  :hook ((racket-mode . lsp)))

(use-package rust-mode
  :ensure t
  :init (setq rust-mode-treesitter-derive t)
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        rust-indent-offset 4)
  :hook ((rust-mode . lsp)))

(use-package svelte-mode
  :ensure t
  :hook ((svelte-mode . lsp)))

(use-package swift-mode
  :ensure t
  :hook ((swift-mode . lsp)))

(use-package typescript-ts-mode
  :hook ((typescript-ts-mode . lsp)))

(use-package tuareg
  :ensure t
  :config
  (setq tuareg-match-patterns-aligned t)
  :hook ((tuareg-mode . lsp)))

(use-package zig-mode
  :ensure t
  :hook ((zig-mode . lsp))
  :config (setq zig-format-on-save nil))

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

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
		enable-recursive-minibuffers t)
  (ivy-mode))

(use-package counsel
  :ensure t
  :config (counsel-mode))

(use-package swiper
  :ensure t
  :after ivy
  :config
  (global-set-key (kbd "C-s") 'swiper))

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
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((asm-mode . lsp)
   (c-ts-mode . lsp)
   (c++-ts-mode . lsp)
   (csharp-mode . lsp)
   (glsl-mode . lsp)
   (gleam-ts-mode . lsp)
   (fortran-mode . lsp)
   (f90-mode . lsp)
   (latex-mode . lsp)
   (odin-ts-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key lsp-mode-map (kbd "C-c l f") #'lsp-format-buffer)
  (setq lsp-enable-on-type-formatting nil
        lsp-enable-snippet nil)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "~/.local/bin/ols")
    				:major-modes '(odin-ts-mode)
    				:server-id 'ols
    				:multi-root t))
  (add-to-list 'lsp-language-id-configuration '(odin-ts-mode . "odin"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "/usr/bin/glsl_analyzer")
					:major-modes '(glsl-mode)
					:server-id 'glsl_analyzer
					:multi-root t))
  (add-to-list 'lsp-language-id-configuration '(glsl-mode . "glsl"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "~/.local/bin/c3-lsp")
                    :major-modes '(c3-ts-mode)
                    :server-id 'c3-lsp
                    :multi-root t))
  (add-to-list 'lsp-language-id-configuration '(c3-ts-mode . "c3"))
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
  (projectile-register-project-type 'zig '("build.zig")
									:project-file "build.zig"
									:compile "zig build"
									:test "zig build test"
									:run "zig build run")
  (projectile-register-project-type 'c3 '("project.json")
                                    :project-file "project.json"
                                    :compile "c3c build"
                                    :run "c3c run")
  (projectile-register-project-type 'gleam '("gleam.toml")
                                    :project-file "gleam.toml"
                                    :compile "gleam build"
                                    :run "gleam run")
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/projects/" "~/playgrounds/" "~/faculdade/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package lsp-pyright
  :ensure t
  :hook (python-ts-mode . (lambda ()
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

(use-package lsp-haskell :ensure t)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-\"") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-:") 'mc/skip-to-previous-like-this))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-night))

(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs
        '(c
          c++
          elixir
          go
          heex
          java
          javascript
          python
          svelte
          typescript
          tsx))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Packages.el ends here
