;;; keybinds --- My General Keybinds  -*- lexical-binding: t; -*-
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
	"c c" '(comment-line :wk "Comment Lines"))

  (good-leader-key
    "l u" '(lsp-ui-imenu :wk "Show imenu entries")))

(use-package sudo-edit
  :ensure t
  :config
  (good-leader-key
	"f u" '(sudo-edit-find-file :wk "Sudo find file")
	"f U" '(sudo-edit :wk "Sudo edit file")))

;;; keybinds.el ends here
