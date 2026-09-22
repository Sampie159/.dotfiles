{ config, pkgs, ... }:

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
    wf-recorder
    wl-clipboard
    wget
    clang
    clang-tools
    gnumake
    nodejs
    tree-sitter
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
    nixd
    nixfmt
    moreutils
    pince
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

    direnv = {
      enable = true;
      enableFishIntegration = true;
      nix-direnv.enable = true;
    };
  };

  gtk = {
    enable = true;
    theme = {
      name = "Arc-Dark";
      package = pkgs.arc-theme;
    };
  };

  home.pointerCursor = {
    enable = true;
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
    size = 24;
    gtk.enable = true;
  };

  dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentry.package = pkgs.pinentry-curses;
    };
  };

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

  xdg.configFile."tmux-plugins.conf".text = with pkgs.tmuxPlugins; ''
    run-shell ${sensible.rtp}
    run-shell ${vim-tmux-navigator.rtp}
    run-shell ${yank.rtp}
    run-shell ${power-theme.rtp}
  '';

  systemd.user.sessionVariables = {
    EDITOR = "nvim";
    TERMINAL = "ghostty";
  };
}
