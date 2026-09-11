;; Guix Home config, VM test only - mirrors home.nix, no separate script.
;;
;; Nix's home.nix symlinks straight into ~/.dotfiles (mkOutOfStoreSymlink):
;; edit a file, `hyprctl reload`, done. Guix Home has no such primitive -
;; home-files-service-type copies each path into the store, then symlinks
;; $HOME to that store copy. So dotfile edits here need a rebuild:
;;   guix home reconfigure home.scm
;; before they take effect. That's the real tradeoff of going Guix-only
;; instead of a plain symlink script.
;;
;; bin/ carries symlinks that point outside the repo (codegraph, odin,
;; c3lsp, ...) - those stay dangling in a fresh VM regardless of how
;; they're provisioned, same as on the real machine. uv/uvx are glibc
;; ELF binaries with a hardcoded /lib64/ld-linux-x86-64.so.2 interpreter,
;; which Guix System doesn't provide at that path - they won't run as-is.
;;
;; Build/run: guix home reconfigure home.scm

(use-modules (gnu home)
             (gnu home services)
             (gnu packages)
             (guix gexp))

(home-environment
  (packages (specifications->packages
             '("git" "tmux" "emacs-pgtk" "ripgrep" "fzf" "zoxide"
               "starship" "bat" "eza" "jq" "wl-clipboard" "playerctl"
               "python-pywal" "lazygit" "btop")))

  (services
   (list
    (simple-service 'dotfiles-xdg-config
                     home-xdg-configuration-files-service-type
                     `(("hypr" ,(local-file "hypr" #:recursive? #t))
                       ("waybar" ,(local-file "waybar" #:recursive? #t))
                       ("rofi" ,(local-file "rofi" #:recursive? #t))
                       ("Kvantum" ,(local-file "Kvantum" #:recursive? #t))
                       ("tmux" ,(local-file "tmux" #:recursive? #t))
                       ("nvim" ,(local-file "nvim" #:recursive? #t))
                       ("emacs" ,(local-file "emacs" #:recursive? #t))
                       ("fish" ,(local-file "fish" #:recursive? #t))
                       ("wal/templates" ,(local-file "templates" #:recursive? #t))))

    (simple-service 'dotfiles-home-files
                     home-files-service-type
                     `((".local/bin" ,(local-file "bin" #:recursive? #t))
                       ("Wallpapers" ,(local-file "Wallpapers" #:recursive? #t))
                       (".gitconfig" ,(local-file ".gitconfig")))))))
