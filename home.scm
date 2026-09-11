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
;; bin/ mixes actual scripts with symlinks to outside the repo (codegraph,
;; odin, c3lsp, ...) and committed glibc ELF binaries (uv, uvx). Only the
;; bash scripts are linked below - the rest wouldn't resolve/run on a
;; fresh Guix VM anyway.
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
                     `((".local/bin/build-llvm" ,(local-file "bin/build-llvm" #:recursive? #t))
                       (".local/bin/mkcf" ,(local-file "bin/mkcf" #:recursive? #t))
                       (".local/bin/mko" ,(local-file "bin/mko" #:recursive? #t))
                       (".local/bin/mywal" ,(local-file "bin/mywal" #:recursive? #t))
                       (".local/bin/ofmt" ,(local-file "bin/ofmt" #:recursive? #t))
                       (".local/bin/swank" ,(local-file "bin/swank" #:recursive? #t))
                       (".local/bin/tms" ,(local-file "bin/tms" #:recursive? #t))
                       ("Wallpapers" ,(local-file "Wallpapers" #:recursive? #t))
                       (".gitconfig" ,(local-file ".gitconfig")))))))
