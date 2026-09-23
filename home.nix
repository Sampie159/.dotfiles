{
  config,
  lib,
  pkgs,
  ...
}:

let
  dots = "${config.home.homeDirectory}/.dotfiles";
  link = path: config.lib.file.mkOutOfStoreSymlink "${dots}/${path}";
in
{
  imports = [
    ./home-manager/nvim.nix
    ./home-manager/emacs.nix
    ./home-manager/waybar.nix
    ./home-manager/fish.nix
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
    playerctl
    alacritty
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
      lfs.enable = true;
      settings = {
        user = {
          name = "Sampie159";
          email = "38163547+Sampie159@users.noreply.github.com";
        };
        init.defaultBranch = "master";
        rerere.enabled = true;
        alias.wccc = "-w -C -C -C";
        column.ui = "auto";
        branch.sort = "-committerdate";
        core.editor = "nvim";
      };
    };

    ghostty = {
      enable = true;
      settings = {
        font-family = "JetBrainsMono Nerd Font";
        font-style = "Medium";
        font-size = 13;
        font-feature = "ss01,ss02,ss03,ss04,ss05,ss06,ss07,ss08,ss09,ss10,liga,calt";
        cursor-invert-fg-bg = true;
        adjust-cursor-thickness = 2;
      };
    };

    tmux = {
      enable = true;
      prefix = "C-Space";
      mouse = true;
      baseIndex = 1;
      escapeTime = 0;
      historyLimit = 50000;
      terminal = "screen-256color";
      plugins = with pkgs.tmuxPlugins; [
        sensible
        vim-tmux-navigator
        yank
        {
          plugin = power-theme;
          extraConfig = "set -g @tmux_power_theme 'moon'";
        }
      ];
      extraConfig = ''
        set -ag terminal-overrides ",xterm-256color:RGB"
        set -g renumber-windows on

        bind -n M-H previous-window
        bind -n M-L next-window

        bind-key -T copy-mode-vi v send-keys -X begin-selection
        bind-key -T copy-mode-vi C-v send-keys -X rectangle-toggle
        bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cancel

        bind '"' split-window -v -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
      '';
    };

    mpv.enable = true;
    bat.enable = true;
    gh = {
      enable = true;
      gitCredentialHelper.enable = false;
    };
    pywal.enable = true;
    jq.enable = true;
    mangohud.enable = true;
    ripgrep.enable = true;
    password-store.enable = true;
    firefox.enable = true;
    lazygit.enable = true;
    neovide.enable = true;

    rofi = {
      enable = true;
      settings.font = "CaskaydiaMono Nerd Font 12";
      theme = lib.mkForce { "@import" = "${config.xdg.cacheHome}/wal/colors-rofi-light.rasi"; };
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

  xdg.configFile."Kvantum/kvantum.kvconfig".text = "theme=KvArcDark\n";

  services = {
    mako = {
      enable = true;
      settings = {
        border-radius = 8;
        default-timeout = 5000;
        width = 500;
        include = "${config.xdg.cacheHome}/wal/colors-mako";
      };
    };

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
    ".config/hypr".source = link "hypr";
    ".config/pypr".source = link "pypr";
  };

  systemd.user.sessionVariables = {
    EDITOR = "nvim";
    TERMINAL = "ghostty";
  };
}
