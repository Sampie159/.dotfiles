;;; config --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq c-default-style "linux"
      c-basic-offset 4
      backup-directory-alist '(("." . "~/.trash")))

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

(set-frame-font "IosevkaTerm Nerd Font 13" nil t)

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

;;; config.el ends here
