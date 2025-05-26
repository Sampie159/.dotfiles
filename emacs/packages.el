;;; packages --- All plugins used in my config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package transient :ensure t)
(use-package magit :ensure t)
(use-package seq :ensure t)
(use-package ripgrep :ensure t)

(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package cmake-mode
  :ensure t
  :hook ((cmake-mode . lsp))
  :config (setq cmake-tab-width 4))

(use-package glsl-mode
  :ensure t
  :hook ((glsl-mode . lsp)))

(use-package rust-mode
  :ensure t
  :hook ((rust-mode . lsp)))

(use-package lua-mode
  :ensure t
  :hook ((lua-mode . lsp)))

(use-package odin-mode
  :ensure (:host sourcehut :repo "mgmarlow/odin-mode")
  :hook ((odin-mode . lsp)))

(use-package parinfer-rust-mode
  :ensure t
  :config (setq parinfer-rust-preferred-mode 'smart)
  :hook (emacs-lisp-mode)
  :init (setq parinfer-rust-auto-download t))

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
        vterm-toggle-scope 'project))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (;;(c-mode .lsp)
   ;;(c++-mode . lsp)
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
  :commands (lsp))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package counsel
  :ensure t
  :config (counsel-mode))

(use-package corfu
  :ensure t
  :init (global-corfu-mode))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extend-command-predicate #'command-completion-default-include-p))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/projects/" "~/playgrounds/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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

(use-package parchment-theme
  :ensure t)
  ;; :config (load-theme 'parchment))

(use-package kaolin-themes
  :ensure t
  :config (load-theme 'kaolin-aurora))

;;; packages.el ends here
