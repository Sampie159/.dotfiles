;;; config --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq c-default-style "linux"
      c-basic-offset 4
      lua-indent-level 4
      sql-indent-offset 4
      tab-width 4
      indent-tabs-mode nil
      backup-directory-alist '(("." . "~/.trash")))

;; treesit ignores `font-lock-maximum-decoration'; 4 is its max and the only
;; level that fontifies function calls, variables, operators and delimiters
;; setopt, not setq -- this var's :set handler is what re-fontifies live buffers
(setopt treesit-font-lock-level 4)
(setq c-ts-mode-indent-style 'linux
      c-ts-mode-indent-offset 4)

(with-eval-after-load 'cc-mode
  (add-hook 'c++-mode-hook
            (lambda ()
              (c-set-offset 'innamespace 0)))
  ;; cc-mode binds TAB to its own `c-indent-line-or-region', which only knows
  ;; `c-tab-always-indent' and so ignores `tab-always-indent' = complete. Put the
  ;; standard command back so TAB completes here like it does in every other mode
  ;; (covers c/c++/objc/java and slang-mode, which derives from c-mode).
  (define-key c-mode-base-map (kbd "TAB") #'indent-for-tab-command))

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; (set-frame-font "JetBrainsMono Nerd Font 11" nil t)
;; (set-frame-font "InconsolataGo Nerd Font 13" nil t)
(set-face-attribute 'default nil
                    :family "Inconsolata Nerd Font"
                    :height 120
                    :weight 'bold)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(setq-default indent-tabs-mode nil
              tab-width 4
              cursor-type 'hollow)

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

(add-hook 'c-mode-common-hook
          (lambda ()
            (header-guard)))

(defun elisp-headandfoot ()
  "Insert the headers and footers and inbetweeners required by elisp."
  (interactive)
  (when (and (buffer-file-name)
             (string-match "\\.el\\'" (buffer-file-name))
             (zerop (buffer-size)))
    (let ((filename (file-name-nondirectory (buffer-file-name))))
      (insert (concat ";;; " filename " --- \n"))
      (insert ";;; Commentary:\n")
      (insert ";;; Code:\n")
      (insert (concat ";;; " filename " ends here"))
      (previous-line 2))))

(add-hook 'emacs-lisp-mode-hook 'elisp-headandfoot)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;;; config.el ends here
