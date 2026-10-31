;;; config --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq c-default-style "linux"
      c-basic-offset 4
      lua-indent-level 4
      sql-indent-offset 4
      backup-directory-alist '(("." . "~/.trash")))

;; treesit ignores `font-lock-maximum-decoration'; 4 is its max and the only
;; level that fontifies function calls, variables, operators and delimiters
;; setopt, not setq -- this var's :set handler is what re-fontifies live buffers
(setopt treesit-font-lock-level 4)
(setq c-ts-mode-indent-style 'linux
      c-ts-mode-indent-offset 4
      go-ts-mode-indent-offset 4
      cmake-ts-mode-indent-offset 4)

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
                    :family "Monaspace Neon Var"
                    :height 110
                    :weight 'bold)

(add-to-list 'default-frame-alist '(alpha-background . 90))

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
      (forward-line -2)
      (end-of-line))))

(add-hook 'emacs-lisp-mode-hook 'elisp-headandfoot)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

(defvar lsp-target--projects (make-hash-table :test #'equal))
(defvar lsp-target--directories nil)

(defun lsp-target--clangd-command (original)
  "Apply the selected target to ORIGINAL's clangd command for this project."
  (let ((command (funcall original))
        (settings (gethash (file-truename (file-name-as-directory default-directory))
                           lsp-target--projects)))
    (if settings
        (append (list "env" (concat "XDG_CONFIG_HOME=" (car settings)))
                command (list (concat "--query-driver=" (cdr settings))))
      command)))

(defun lsp-target (&optional target)
  "Toggle the current clangd workspace between Linux and Windows.
With a prefix argument, prompt for TARGET.  Project compile flags and
unsaved buffers are preserved.  The selection lasts for this Emacs session."
  (interactive
   (list (when current-prefix-arg
           (intern (completing-read "Clangd target: " '("linux" "windows") nil t)))))
  (require 'lsp-mode)
  (let ((workspace (seq-find
                     (lambda (workspace)
                       (eq (lsp--client-server-id (lsp--workspace-client workspace)) 'clangd))
                     (lsp-workspaces))))
    (unless (and workspace (eq (lsp--workspace-status workspace) 'initialized))
      (user-error "Wait for clangd to attach to a C/C++ buffer"))
    (when (file-remote-p (lsp--workspace-root workspace))
      (user-error "Target switching requires a local clangd workspace"))
    (let* ((root (file-truename (file-name-as-directory (lsp--workspace-root workspace))))
           (current (if (gethash root lsp-target--projects) 'windows 'linux))
           (target (or target (if (eq current 'linux) 'windows 'linux))))
      (unless (memq target '(linux windows))
        (user-error "Target must be linux or windows"))
      (unless (eq target current)
        (if (eq target 'windows)
            (let ((compiler (executable-find "x86_64-w64-mingw32-gcc")))
              (unless compiler
                (user-error "Windows target needs x86_64-w64-mingw32-gcc on PATH"))
              (let* ((directory (make-temp-file "emacs-clangd-windows-" t))
                     (user-config (expand-file-name "clangd/config.yaml"
                                                    (or (getenv "XDG_CONFIG_HOME") "~/.config"))))
                (push directory lsp-target--directories)
                (make-directory (expand-file-name "clangd" directory))
                ;; Override only the parser target, retaining project and user flags.
                (with-temp-file (expand-file-name "clangd/config.yaml" directory)
                  (when (file-readable-p user-config)
                    (insert-file-contents user-config)
                    (goto-char (point-max)))
                  (insert "\n---\n"
                          (json-serialize
                           `((CompileFlags .
                              ((Compiler . ,compiler)
                               (Remove . ["--target=*" "-target" "--sysroot=*"
                                          "-isysroot" "--gcc-toolchain=*"])))))
                          "\n"))
                (puthash root (cons directory compiler) lsp-target--projects)))
          (remhash root lsp-target--projects))
        (lsp-workspace-restart workspace))
      (message "clangd target: %s (current workspace)" target))))

(defun lsp-target--cleanup ()
  "Remove private clangd configs when Emacs exits."
  (dolist (directory lsp-target--directories)
    (when (file-directory-p directory)
      (delete-directory directory t))))

(add-hook 'kill-emacs-hook #'lsp-target--cleanup)
(with-eval-after-load 'lsp-clangd
  (advice-add 'lsp-clients--clangd-command :around #'lsp-target--clangd-command))
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l t") #'lsp-target))

;;; config.el ends here
