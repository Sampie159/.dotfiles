{ config, pkgs, ... }:

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
        wl-clipboard
        btop
        vesktop
        telegram-desktop

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
    };
}
