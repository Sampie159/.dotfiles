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
  :init (add-to-list 'auto-mode-alist '("\\.gleam\\'" . gleam-ts-mode))
  :hook ((gleam-ts-mode . lsp)))

(use-package go-mode
  :ensure t
  :hook
  ((go-mode . lsp)
   (before-save . lsp-organize-imports))
  :init (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :config
  (setq go-mode-indent-offset 4))

(use-package haskell-mode
  :ensure t
  :config (setq lsp-haskell-formatting-provider "stylish-haskell")
  :hook ((haskell-mode . lsp)))

(use-package odin-mode
  :ensure (:host github :repo "Sampie/odin-mode")
  :hook ((odin-mode . lsp)))

(use-package racket-mode
  :ensure t
  :hook ((racket-mode . lsp)))

(use-package rust-mode
  :ensure t
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        rust-indent-offset 4)
  :hook ((rust-mode . lsp)))

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
  :config (setq zig-format-on-save nil)
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

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
		enable-recursive-minibuffers t)
  (ivy-mode))

(use-package counsel
  :ensure t
  :config (counsel-mode))

;; (use-package swiper
;;   :ensure t
;;   :after ivy
;;   :config
;;   (global-set-key (kbd "C-s") 'swiper))

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
  ((c3-ts-mode .lsp)
   (csharp-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key lsp-mode-map (kbd "C-c l f") #'lsp-format-buffer)
  (setq lsp-enable-on-type-formatting nil
        lsp-enable-snippet nil
        lsp-inlay-hint-enable nil
        lsp-diagnostics-provider :none)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "~/.local/bin/ols")
    				:major-modes '(odin-mode)
    				:server-id 'ols
    				:multi-root t))
  (add-to-list 'lsp-language-id-configuration '(odin-mode . "odin"))
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

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-keyword-faces
		'(("TODO" . "#E6E600")
		  ("DEBUG" . "#A020F0")
		  ("FIXME" . "#FF0000")
		  ("NOTE" . "#FF4500")
		  ("DEPRECATED" . "#1E90FF"))
        hl-todo-highlight-punctuation ":")
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

(use-package edit-indirect :ensure t)

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

;; (use-package smart-tab
;;   :ensure t
;;   :config (global-smart-tab-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-ts-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

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

(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package corfu
  :ensure t
  :init (global-corfu-mode))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("M-TAB" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package circe
  :ensure t
  :config
  (setq circe-network-options
        '(("CLONKOZONE"
           :host "colonq.computer" :port 26697
           :tls t
           :nick "sampie"))))

;;; packages.el ends here
