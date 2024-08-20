;;; configs --- A bunch of configs
;;; Commentary:
;;; No idea why it's in this file but ok

;;; Code:

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
		"--compile-commands-dir=./debug"
        "--background-index"
        "--completion-style=bundled"
        "--pch-storage=memory"
        "--header-insertion=never"
        "--header-insertion-decorators=0"))

(require 'org-tempo)
(org-babel-do-load-languages
 'org-babel-load-languages ' ((C . t)))

(icomplete-mode 1)

(add-hook 'c++-mode-hook #'(lambda () (c-set-offset 'innamespace [0])))

(set-frame-font "CaskaydiaMono Nerd Font 11" nil t)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

;; (let ((time (string-to-number (format-time-string "%H" (current-time)))))
;;   (if (or (> time 17) (< time 9))
;; 	  (load-theme 'sdark)
;; 	(load-theme 'slight)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(load-file "~/.config/emacs/glsl-mode.el")

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.tesc\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.tese\\'" . glsl-mode))

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-x b") 'counsel-switch-buffer)
	map)
  "Defines the my-keys-minor-mode keymaps.")

(define-minor-mode my-keys-minor-mode
  "Redefining some keys."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(setq display-buffer-alist
	  '(("\\*compilation\\*"
		 (display-buffer-reuse-window
		  display-buffer-below-selected)
		 (window-height . 15)
		 (dedicated . t)
		 (body-function . (lambda (window) (select-window window))))))

(defvar treesit-language-source-alist
      '((c3 "https://github.com/c3lang/tree-sitter-c3")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript")))

(defun treesit-install-all-grammars ()
  "Install all the language grammars defined."
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; (setq major-mode-remap-alist
;;       '((c-mode . c-ts-mode)
;;         (c++-mode . c++-ts-mode)
;;         (c-or-c++-mode . c-or-c++-ts-mode)
;;         (csharp-mode . csharp-ts-mode)
;;         (python-mode . python-ts-mode)
;;         (typescript-mode . typescript-ts-mode)))

;;; configs.el ends here
