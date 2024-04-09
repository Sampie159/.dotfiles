;;; package --- use-packages
;;; Commentary:
;; Holds all the packages that I find necessary

;;; Code:
(use-package seq)

(use-package ripgrep)

(use-package transient)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package cmake-mode
  :hook ((cmake-mode . lsp)))

(use-package rust-mode
  :ensure t
  :config (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  :hook ((rust-mode . lsp)))

(use-package go-mode
  :ensure t
  :hook
  ((go-mode . lsp)
   (before-save . lsp-organize-imports)))

;; (use-package odin-mode
;;   :ensure (:host github :repo "Sampie159/odin-mode")
;;   :hook ((odin-mode . lsp)))

;; (use-package php-mode
;;   :hook ((php-mode . lsp)))

(use-package slime
  :config (setq inferior-lisp-program "sbcl"))

(use-package svelte-mode
  :hook ((svelte-mode . lsp)))

(use-package typescript-mode
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
  (setq shell-file-name "/bin/zsh"
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
  ((c-mode . lsp)
   (c++-mode . lsp)
   (csharp-mode . lsp)
   (fortran-mode . lsp)
   (f90-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-enable-on-type-formatting nil)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "/usr/bin/ols")
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

(use-package naysayer-theme
 :config (load-theme 'naysayer))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp))))

(use-package company)

;;; packages.el ends here
