;;; keybinds --- My General Keybinds
;;; Commentary:
;;; Code:

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer evil-leader-key
    :prefix "SPC")
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
			(newline-and-indent)
			(previous-line)
			(indent-according-to-mode)))
   
   "M-p" '(lambda () (interactive)
	    (transpose-lines 1)
	    (forward-line -2))
   
   "M-n" '(lambda () (interactive)
	    (forward-line 1)
	    (transpose-lines 1)
	    (forward-line -1)))
  
  (good-leader-key
	:keymaps 'normal
	"f c" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :wk "Edit emacs config")
	"e s" '(eshell :wk "Eshell")
	"t v" '(vterm-toggle :wk "Toggle vterm")
	"h f" '(describe-function :wk "Describe function")
	"h v" '(descrive-variable :wk "Describe variable")
	"h r r" '((lambda () (interactive)
   				(load-file "~/.config/emacs/init.el")
   				(load-file "~/.config/emacs/init.el"))
   			  :wk "Reload emacs config"))

  (evil-leader-key
	:keymaps 'normal
    "l u" '(lsp-ui-imenu :wk "Show imenu entries"))

  (general-nmap "gcc" '(comment-line :wk "Comment lines")))

(use-package sudo-edit
  :ensure t
  :config
  (evil-leader-key
	:keymaps 'normal
	"f u" '(sudo-edit-find-file :wk "Sudo find file")
	"f U" '(sudo-edit :wk "Sudo edit file")))

;;; keybinds.el ends here
