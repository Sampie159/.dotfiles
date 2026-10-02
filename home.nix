{
  config,
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
    ./home-manager/quickshell.nix
    ./home-manager/fish.nix
    ./home-manager/hyprland.nix
  ];

  _module.args.link = link;

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
    libqalculate
    virt-manager
    blender
    gimp
    devenv
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

    ghostty.enable = true;

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
    neovide.enable = true;

    lazygit = {
      enable = true;
      settings.customCommands = [
        {
          key = "G";
          context = "global";
          description = "Create GitHub repo and push";
          prompts = [
            {
              type = "input";
              title = "Repo name";
              key = "Name";
            }
            {
              type = "menu";
              title = "Visibility";
              key = "Visibility";
              options = [
                { value = "private"; }
                { value = "public"; }
              ];
            }
          ];
          command = "gh repo create {{.Form.Name}} --{{.Form.Visibility}} --source=. --remote=origin --push";
          output = "terminal";
        }
      ];
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
      icons = "auto";
      git = true;
      colors = "auto";
      extraOptions = [
        "--group-directories-first"
        "--header"
      ];
    };

    fzf = {
      enable = true;
      enableFishIntegration = false;
    };

    direnv = {
      enable = true;
      enableFishIntegration = true;
      config.global.hide_env_diff = true;
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
  dconf.settings."org/virt-manager/virt-manager/connections" = {
    autoconnect = [ "qemu:///system" ];
    uris = [ "qemu:///system" ];
  };

  xdg.configFile."Kvantum/kvantum.kvconfig".text = "theme=KvArcDark\n";
  xdg.configFile."ghostty/config".source = link "ghostty/config";

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
  };

  systemd.user.sessionVariables = {
    EDITOR = "nvim";
    TERMINAL = "ghostty";
  };
}
