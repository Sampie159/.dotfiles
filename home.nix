{ config, pkgs, inputs, ... }:

let
    dots = "${config.home.homeDirectory}/.dotfiles";
    link = path: config.lib.file.mkOutOfStoreSymlink "${dots}/${path}";
in
{
    home.username = "sampie";
    home.homeDirectory = "/home/sampie";
    home.stateVersion = "26.05";

    programs.home-manager.enable = true;

    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
        fastfetch
        tree
        killall
        pavucontrol
        pyprland
        grim
        slurp
        meson
        ninja
        python3
        wl-clipboard
        spotify
        wget
        clang
        llvm
        rustup
        cmake
        gnumake
        nodejs
        tree-sitter
        libtool
        pkg-config
        sccache
        unrar
        p7zip
        libnotify
        btop
        tmux
        discord
        telegram-desktop
        protonup-qt
        aseprite
        obs-studio
        nautilus
        wineWow64Packages.staging
        winetricks
        qbittorrent
        awww
        pywalfox-native
        mako
        rofi
        waybar
        ghostty
        playerctl

        # config.fish inits these directly
        zoxide
        starship

        inputs.zig.packages.${pkgs.stdenv.hostPlatform.system}.master
        inputs.neovim-nightly-overlay.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];

    programs = {
        git = {
            enable = true;
            settings.user = {
                name = "Sampie159";
                email = "38163547+Sampie159@users.noreply.github.com";
            };
        };

        mpv.enable = true;
        bat.enable = true;
        gh.enable = true;
        pywal.enable = true;
        jq.enable = true;
        mangohud.enable = true;
        ripgrep.enable = true;
        password-store.enable = true;
        firefox.enable = true;
        lazygit.enable = true;
        neovide.enable = true;

        emacs = {
            enable = true;
            package = pkgs.emacs-pgtk;
        };

        irssi = {
            enable = true;
            networks.clonk = {
                nick = "sampie";
                server = {
                    address = "colonq.computer";
                    port = 26697;
                    autoConnect = true;
                    ssl.enable = true;
                    ssl.verify = true;
                };
            };
        };

        eza = {
            enable = true;
            enableFishIntegration = false;
        };

        fzf = {
            enable = true;
            enableFishIntegration = false;
        };
    };

    gtk = {
        enable = true;
        theme = {
            name = "Arc-Dark";
            package = pkgs.arc-theme;
        };
    };

    services = {
        gpg-agent = {
            enable = true;
            enableSshSupport = true;
            pinentry.package = pkgs.pinentry-curses;
        };
    };

    # All config dirs symlink live to ~/.dotfiles (like the Arch install.sh
    # model): edits apply on reload, no rebuild, and untracked files still
    # show up.
    home.file = {
        ".config/hypr".source = link "hypr";
        ".config/waybar".source = link "waybar";
        ".config/rofi".source = link "rofi";
        ".config/ghostty".source = link "ghostty";
        ".config/Kvantum".source = link "Kvantum";
        ".config/tmux".source = link "tmux";
        ".config/nvim".source = link "nvim";
        ".config/emacs".source = link "emacs";
        ".config/fish".source = link "fish";
        ".config/wal/templates".source = link "templates";
        ".local/bin".source = link "bin";
        "Wallpapers".source = link "Wallpapers";
        ".tmux/plugins/tpm".source = pkgs.fetchFromGitHub {
            owner = "tmux-plugins";
            repo = "tpm";
            rev = "v3.1.0";
            # regen on rev bump: nix-hash --type sha256 --sri <unpacked tarball dir>
            hash = "sha256-CeI9Wq6tHqV68woE11lIY4cLoNY8XWyXyMHTDmFKJKI=";
        };
    };

    systemd.user.sessionVariables = {
        CC = "clang";
        CXX = "clang++";
        EDITOR = "nvim";
        TERMINAL = "ghostty";
    };
}
