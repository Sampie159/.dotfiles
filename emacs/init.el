;;; init --- Entry point for my configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq font-lock-maximum-decoration t
      inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-font-lock-mode 1)

(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

(setq custom-file "~/.config/emacs/custom-file.el")

(load custom-file 'noerror)

(setq-default lsp-auto-guess-root t)

(setq packages "~/.config/emacs/packages.el")
(load-file packages)

(setq config "~/.config/emacs/config.el")
(load-file config)

;;; init.el ends here

