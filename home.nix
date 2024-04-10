{ config, pkgs, zig, ... }:

{
    home.username = "sampie";
    home.homeDirectory = "/home/sampie";


    fonts.fontconfig.enable = true;
    home.packages = with pkgs; [
        neofetch
        tree
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
        btop
        vesktop
        telegram-desktop
        kanata
        zig.packages."${pkgs.system}".master

        (pkgs.nerdfonts.override { fonts = [ "FiraCode" "CascadiaMono" ]; })
    ];

    wayland.windowManager.hyprland = {
        enable = true;
    };

    programs = {
        git = {
            enable = true;
            userName = "Sampie159";
            userEmail = "sfamboni@gmail.com";
        };

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
                hype = "Hyprland";

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
                tls = "tmus ls";

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
                zbr = "zig build -Doptimize=Release";
                zrr = "zig build -Doptimize=Release run";
            };
        };

        eza = {
            enable = true;
            enableFishIntegration = true;
        };

        fzf = {
            enable = true;
            enableFishIntegration = true;
        };

        rofi = {
            enable = true;
            font = "CaskaydiaMono Nerd Font 12";
        };
    };

    services = {
        mako = {
            enable = true;
            font = "CaskaydiaMono Nerd Font 10";
            borderRadius = 8;
            defaultTimeout = 5000;
            width = 500;
            extraConfig = ''
            background-color=#00000000
            text-color=#000000
            border-color=#000000
            '';
        };
    };

    home.file = {
        ".config/alacritty".source = ./alacritty;
        ".config/hypr".source = ./hypr;
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
