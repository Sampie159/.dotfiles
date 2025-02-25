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

(set-frame-font "Iosevka Nerd Font 13" nil t)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
;; (load-theme 'assemblage)

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
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'label)
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

(defun header-guard ()
  "Add header guard to new C/C++ header files."
  (when (and (buffer-file-name)
             (string-match "\\.\\(h\\|hh\\|hpp\\)\\'" (buffer-file-name))
             (zerop (buffer-size)))
    (let* ((filename (file-name-nondirectory (buffer-file-name)))
           (extension (concat "_" (upcase (file-name-extension filename)) "_"))
           (guard (concat "_" (upcase (file-name-sans-extension filename)) extension)))
      (insert "#if !defined(" guard ")\n\n\n\n#define " guard "\n")
      (insert "#endif /* " guard " */\n")
      (goto-char (point-min))
      (forward-line 2))))

(defun label (label)
  (interactive "sLABEL: ")
  (insert "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */\n")
  (insert (concat "/*" (store-substring (make-string 75 ?\s) (/ (- 75 (length label)) 2) (upcase label))) "*/\n")
  (insert "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */\n"))

(defun insert-if0-guard ()
  "Insert a #if 0 ... #endif block around a block."
  (interactive)
  (save-excursion
    (let ((begin (progn (beginning-of-defun) (point)))
          (end (progn (end-of-defun) (point))))
      (goto-char end)
      (insert "#endif\n")
      (goto-char begin)
      (insert "#if 0\n"))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'inextern-lang 0)
            (c-set-offset 'innamespace 0)
            (header-guard)
            (define-key c-mode-map (kbd "C-c 0") 'insert-if0-guard)))
            ;; (define-key c-mode-map (kbd "C-c C-l") 'label)))

(defun elisp-headandfoot ()
  "Insert the headers and footers and inbetweeners required by elisp."
  (interactive)
  (when (and (buffer-file-name)
             (string-match "\\.el\\'" (buffer-file-name))
             (zerop (buffer-size)))
    (let ((filename (file-name-nondirectory (buffer-file-name))))
      (insert (concat ";;; " filename " --- \n"))
      (insert ";;; Commentary:\n")
      (insert ";;; Code:\n\n\n\n")
      (insert (concat ";;; " filename " ends here"))
      (previous-line 2))))

(add-hook 'emacs-lisp-mode-hook 'elisp-headandfoot)
(add-hook 'c3-ts-mode-hook
          (lambda()
            (when (zerop (buffer-size))
              (insert "\n\n")
              (goto-char (point-min)))))

(setq-default cursor-type 'hollow)

;;; configs.el ends here
