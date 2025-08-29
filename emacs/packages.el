;;; packages --- All plugins used in my config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package general
  :ensure t
  :config
  (general-create-definer good-leader-key
    :prefix "C-c")
  (general-define-key
   "C-<return>" '(lambda () (interactive)
                   (let ((oldpos (point)))
                     (end-of-line)
                     (newline-and-indent)))

   "C-S-<return>" '(lambda () (interactive)
                     (let ((oldpos (point)))
                       (beginning-of-line)
                       (newline)
                       (previous-line)
                       (indent-according-to-mode)))

   "M-p" '(lambda () (interactive)
            (transpose-lines 1)
            (forward-line -2))

   "M-n" '(lambda () (interactive)
            (forward-line 1)
            (transpose-lines 1)
            (forward-line -1))

   "C-l" '(lambda () (interactive)
            (move-beginning-of-line 1)
            (kill-line)
            (yank)
            (open-line 1)
            (next-line 1)
            (yank))
   "C-," '(lambda () (interactive)
            (recenter-top-bottom)))

  (good-leader-key
   "f c" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :wk "Edit emacs config")
   "e s" '(eshell :wk "Eshell")
   "t v" '(vterm-toggle :wk "Toggle vterm")
   "h f" '(describe-function :wk "Describe function")
   "h v" '(describe-variable :wk "Describe variable")
   "h r r" '((lambda () (interactive)
               (load-file "~/.config/emacs/init.el")
               (load-file "~/.config/emacs/init.el"))
             :wk "Reload emacs config")
   "l u" '(lsp-ui-imenu :wk "Show imenu entries")
   "c c" '(comment-line :wk "Comment lines")))

(use-package sudo-edit
  :ensure t
  :config
  (good-leader-key
   "f u" '(sudo-edit-find-file :wk "Sudo find file")
   "f U" '(sudo-edit :wk "Sudo edit file")))

(use-package transient :ensure t)
(use-package magit :ensure t)
(use-package seq :ensure t)
(use-package ripgrep :ensure t)

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

(use-package zig-mode
  :ensure t
  :config
  (setq zig-format-on-save nil)
  :hook ((zig-mode . lsp)))

(use-package odin-ts-mode
  :ensure (:host github :repo "Sampie159/odin-ts-mode")
  :mode "\\.odin\\'"
  :hook ((odin-ts-mode . lsp)))

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
  ((c-mode .lsp)
   (c++-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key lsp-mode-map (kbd "C-c l f") #'lsp-format-buffer)
  (setq lsp-enable-on-type-formatting nil
        lsp-enable-snippet nil
        lsp-inlay-hint-enable nil
        lsp-diagnostics-provider :none)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "ols")
                    :major-modes '(odin-ts-mode)
                    :server-id 'ols
                    :multi-root t))
  (add-to-list 'lsp-language-id-configuration '(odin-ts-mode . "odin"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "glsl_analyzer")
                    :major-modes '(glsl-mode)
                    :server-id 'glsl-analyzer
                    :multi-root t))
  (add-to-list 'lsp-language-id-configuration '(glsl-mode . "glsl"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "zls")
                    :major-modes '(zig-mode)
                    :server-id 'zls
                    :multi-root t))
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
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
;;  :config (load-theme 'parchment))

(use-package kaolin-themes
  :ensure t)
;;  :config (load-theme 'kaolin-aurora))

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-shell-name "/usr/bin/fish")
  :config
  (exec-path-from-shell-initialize))

;;; packages.el ends here
