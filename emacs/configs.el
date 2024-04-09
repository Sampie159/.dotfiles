(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

;; C/C++ and alike languages
(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-common-hook
    (lambda ()
      (local-set-key (kbd "C-c o") 'ff-find-other-file)))

(setq backup-directory-alist '((".*" . "~/.trash")))
(setq lsp-clients-clangd-args
      '("-j=16"
        "--background-index"
        "--completion-style=bundled"
        "--pch-storage=memory"
        "--header-insertion=never"
        "--header-insertion-decorators=0"))

(require 'org-tempo)
(org-babel-do-load-languages
 'org-babel-load-languages ' ((C . t)))

(icomplete-mode 1)

(add-hook 'c++-mode-hook '(lambda () (c-set-offset 'innamespace [0])))

;; (set-face-attribute
;;  'default
;;  (selected-frame)
;;  :font "FiraCode Nerd Font"
;;  :height 130)

(set-frame-font "CaskaydiaMono Nerd Font 11" nil t)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
;; (load-theme 'sampie)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

;;; configs.el ends here
