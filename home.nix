{ config, pkgs, inputs, ... }:

let
    dots = "${config.home.homeDirectory}/.dotfiles";
    link = path: config.lib.file.mkOutOfStoreSymlink "${dots}/${path}";
in
{
    imports = [
        ./home-manager/nvim.nix
        ./home-manager/emacs.nix
    ];

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
        alsa-utils
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
        clang-tools
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
        playerctl
        tmux
        rofi
        ghostty
        alacritty
        waybar
        zoxide
        starship
        gnupg
        vulkan-tools
        dropbox

        inputs.zig.packages.${pkgs.stdenv.hostPlatform.system}.master
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

    # GTK4/libadwaita apps (Nautilus, etc.) don't read gtk.theme.name or
    # GTK_THEME at all - only this dconf key, which gtk.theme never sets.
    # Qt's already dark at the system level (configuration.nix's qt.style).
    dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";

    services = {
        gpg-agent = {
            enable = true;
            enableSshSupport = true;
            pinentry.package = pkgs.pinentry-curses;
        };
    };

    # Real symlinks straight to the repo, same as install.sh's `ln -sf
    # ~/.dotfiles/<name> ~/.config/` on the live Arch config (master branch) -
    # not Nix-generated/copied-into-the-store config. These configs already
    # work standalone; Nix's job is package installs, nothing more. Editing
    # a dotfile takes effect immediately, no `home-manager switch` needed.
    home.file = {
        ".config/wal/templates".source = link "templates";
        ".local/bin".source = link "bin";
        "Wallpapers".source = link "Wallpapers";

        ".config/nvim".source = link "nvim";
        ".config/emacs".source = link "emacs";
        ".config/tmux".source = link "tmux";
        ".config/ghostty".source = link "ghostty";
        ".config/hypr".source = link "hypr";
".config/pypr".source = link "pypr";
        ".config/waybar".source = link "waybar";
        ".config/rofi".source = link "rofi";
        ".config/fish".source = link "fish";
        ".config/Kvantum".source = link "Kvantum";
    };

    systemd.user.sessionVariables = {
        CC = "clang";
        CXX = "clang++";
        EDITOR = "nvim";
        TERMINAL = "ghostty";
        RUSTC_WRAPPER = "sccache";
    };
}
