;; Initial configuration for Emacs.
;; Turn off useless UI elements
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

(elpaca nil (message "deferred"))

(defconst custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(load custom-file t)

(setq configs "~/.config/emacs/configs.el")
(load-file configs)

;; Language stuff
(setq-default lsp-auto-guess-root t)
(defvar lsp-language-id-configuration '((c-mode . "c")
                                        (c++-mode . "cpp")
                                        (cmake-mode . "cmake")
                                        (csharp-mode . "csharp")
                                        (glsl-mode . "glsl")
                                        (f90-mode . "fortran")
                                        (go-mode . "go")
                                        (julia-mode . "julia")
                                        (php-mode . "php")
                                        (python-mode . "python")
                                        (svelte-mode . "svelte")
                                        (tuareg-mode . "ocaml")
                                        (typescript-mode . "typescript")
                                        (odin-mode . "odin")
                                        (rust-mode . "rust")
                                        (zig-mode . "zig")))

;; Installed packages found here
(setq packages-file "~/.config/emacs/packages.el")
(load-file packages-file)

;; Keybindings
(setq keybindings "~/.config/emacs/keybinds.el")
(load-file keybindings)
