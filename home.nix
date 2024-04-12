{ config, pkgs, zig, ... }:

{
    home.username = "sampie";
    home.homeDirectory = "/home/sampie";
    home.keyboard = {
        model = "br,us";
        variant = "abnt2,";
        options = [ "grp:ctrls_toggle" ];
    };

    fonts.fontconfig.enable = true;
    home.packages = with pkgs; [
        neofetch
        tree
        killall
        pavucontrol
        steam
        gamemode
        alacritty
        pyprland
        grim
        slurp
        meson
        ninja
        python3
        wl-clipboard
        spotify
        neovim
        wget
        # gcc
        clang
        llvm
        rustup
        cmake
        gnumake
        nodejs
        tree-sitter
        libtool
        pkg-config
        btop
        vesktop
        telegram-desktop
        protonup-qt
        kanata
        lutris
        erlang
        gleam
        elixir
        aseprite
        wine-staging
        wine
        winetricks
        qbittorrent
        swww
        pywalfox-native
        mako
        rofi
        playerctl
        zig.packages."${pkgs.system}".master

        (pkgs.nerdfonts.override { fonts = [ "FiraCode" "CascadiaMono" ]; })
    ];

    programs = {
        git = {
            enable = true;
            userName = "Sampie159";
            userEmail = "sfamboni@gmail.com";
        };

        mpv.enable = true;
        bat.enable = true;
        gh.enable = true;
        pywal.enable = true;
        jq.enable = true;
        emacs.enable = true;
        tmux.enable = true;
        mangohud.enable = true;
        ripgrep.enable = true;
        password-store.enable = true;
        waybar.enable = true;
        firefox.enable = true;
        lazygit.enable = true;

        irssi = {
            enable = true;
            networks = {
                clonk = {
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
        };
        
        fish = {
            enable = true;
            shellAliases = {
                # General aliases
                nv = "nvim";
                po = "poweroff";
                rb = "reboot";
                sd = "shutdown now";
                hx = "helix";
                ls = "eza";
                cat = "bat";
                hypr = "Hyprland";

                # Rust aliases
                ca = "cargo add";
                cb = "cargo build";
                cbr = "cargo build --release";
                cbp = "cargo build --profile";
                cr = "cargo run";
                crr = "cargo run --release";
                crp = "cargo run --profile";
                cw = "cargo watch -x";
                cwb = "cargo watch -x build";
                cwr = "cargo watch -x run";
                cwt = "cargo watch -x test";

                # Tmux aliases
                t = "tmux";
                ta = "tmux attach -t";
                tns = "tmux new -s";
                tks = "tmux kill-session";
                tls = "tmux ls";

                # Meson aliases
                min = "meson init build";
                ms = "meson setup build";
                msw = "meson setup --wipe build";
                mcb = "meson compile -C build";
                mswcb = "meson setup --wipe build && meson compile -C build";

                # CMake aliases
                cmin = "cmake -S . -B debug -DCMAKE_BUILD_TYPE=Debug -G Ninja";
                cmd = "cmake --build debug";
                cmi = "sudo cmake --install release --prefix /usr/local";
                cminr = "cmake -S . -B release -DCMAKE_BUILD_TYPE=Release -G Ninja";
                cmr = "cmake --build release";

                # Zig aliases
                zb = "zig build -Doptimize=Debug";
                zr = "zig build -Doptimize=Debug run";
                zbr = "zig build -Doptimize=ReleaseFast";
                zrr = "zig build -Doptimize=ReleaseFast run";
            };
            shellInit = ''
            cat ~/.cache/wal/sequences
            '';
        };

        eza = {
            enable = true;
            enableFishIntegration = true;
        };

        fzf = {
            enable = true;
            enableFishIntegration = true;
        };
    };

    services = {
        gpg-agent = {
            enable = true;
            enableFishIntegration = true;
            enableSshSupport = true;
            pinentryPackage = pkgs.pinentry-curses;
        };
    };

    home.file = {
        ".config/alacritty".source = ./alacritty;
        ".config/hypr".source = ./hypr;
        ".config/wal/templates".source = ./templates;
        ".config/waybar" = {
            recursive = true;
            source = ./waybar;
        };
        ".config/nvim" = {
            recursive = true;
            source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/nvim";
        };
        ".config/emacs" = {
            recursive = true;
            source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/emacs";
        };
        ".config/tmux" = {
            recursive = true;
            source = ./tmux;
        };
        ".config/rofi/config.rasi".text = ''
        configuration {
        font: "CaskaydiaMono Nerd Font 12";
        }
        @import "~/.cache/wal/colors-rofi-light"
        '';
        "Wallpapers" = {
            recursive = true;
            source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/Wallpapers";
        };
        ".local/state/nix/profile/bin" = {
            recursive = true;
            source = ./bin;
        };
    };

    systemd.user.sessionVariables = {
        CC = "clang";
        CXX = "clang++";
        EDITOR = "nvim";
    };
}
