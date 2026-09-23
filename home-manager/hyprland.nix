{ lib, pkgs, ... }:

let
  inline = lib.generators.mkLuaInline;
  toLua = lib.generators.toLua { };

  mod = "SUPER";
  execDsp = cmd: "hl.dsp.exec_cmd(${toLua cmd})";
  bind = key: dsp: {
    _args = [
      key
      (inline dsp)
    ];
  };
  bindOpts = opts: key: dsp: {
    _args = [
      key
      (inline dsp)
      opts
    ];
  };
  exec = key: cmd: bind key (execDsp cmd);
  locked = key: cmd: bindOpts { locked = true; } key (execDsp cmd);
  scratch = key: name: exec key "pypr toggle ${name} && hyprctl dispatch bringactivetotop";

  directions = {
    H = "left";
    L = "right";
    K = "up";
    J = "down";
  };

  autostart = [
    "pypr"
    "awww-daemon"
    "mywal -r"
    "quickshell"
    "discord --enable-wayland-ime"
    "dropbox"
  ];

  scratchpad = cmd: {
    animation = "fromTop";
    command = cmd;
    lazy = true;
  };
in
{
  home.packages = [ pkgs.hyprpicker ];

  wayland.windowManager.hyprland = {
    enable = true;
    package = null;
    portalPackage = null;

    settings = {
      # require() not dofile(): Hyprland watches required files, so pywal rewrites reload the config
      wal._var = inline ''(function() local ok, t = pcall(require, "~/.cache/wal/colors-hyprland.lua") return ok and type(t) == "table" and t or {} end)()'';

      monitor = {
        output = "";
        mode = "2560x1440@180.00";
        position = "auto";
        scale = 1;
      };

      env = map (e: { _args = e; }) [
        [
          "XCURSOR_SIZE"
          "24"
        ]
        [
          "XCURSOR_THEME"
          "Adwaita"
        ]
        [
          "GTK_THEME"
          "Arc-Dark"
        ]
        [
          "QT_QPA_PLATFORMTHEME"
          "qt5ct"
        ]
        [
          "QT_STYLE_OVERRIDE"
          "kvantum"
        ]
      ];

      on._args = [
        "hyprland.start"
        (inline "function()\n${lib.concatMapStrings (c: "  hl.exec_cmd(${toLua c})\n") autostart}end")
      ];

      config = {
        input = {
          kb_layout = "us,br";
          kb_variant = ",abnt2";
          kb_model = "";
          kb_options = "grp:ctrls_toggle";
          kb_rules = "";
          follow_mouse = 1;
          sensitivity = 0;
          touchpad.natural_scroll = false;
        };

        general = {
          gaps_in = 4;
          gaps_out = 8;
          border_size = 2;
          col = {
            active_border = "rgba(ffffffee)";
            inactive_border = inline ''wal.color11 or "rgb(5056AB)"'';
          };
          layout = "dwindle";
        };

        decoration = {
          rounding = 8;
          blur = {
            enabled = true;
            size = 3;
            passes = 1;
          };
          shadow = {
            enabled = true;
            color = "rgba(1a1a1aee)";
          };
        };

        animations.enabled = true;
        dwindle.preserve_split = true;
      };

      curve._args = [
        "myBezier"
        {
          type = "bezier";
          points = [
            [
              0.05
              0.9
            ]
            [
              0.1
              1.05
            ]
          ];
        }
      ];

      animation = [
        {
          leaf = "windows";
          enabled = true;
          speed = 7;
          bezier = "myBezier";
        }
        {
          leaf = "windowsOut";
          enabled = true;
          speed = 7;
          bezier = "default";
          style = "popin 80%";
        }
        {
          leaf = "border";
          enabled = true;
          speed = 10;
          bezier = "default";
        }
        {
          leaf = "borderangle";
          enabled = true;
          speed = 8;
          bezier = "default";
        }
        {
          leaf = "fade";
          enabled = true;
          speed = 7;
          bezier = "default";
        }
        {
          leaf = "workspaces";
          enabled = true;
          speed = 6;
          bezier = "default";
        }
      ];

      window_rule = [
        {
          name = "workspace-1";
          match.class = "(firefox)$";
          workspace = "1";
        }
        {
          name = "workspace-2";
          match.class = "(discord)$";
          workspace = "2";
        }
        {
          name = "workspace-3";
          match.class = "(steam|lutris)$";
          workspace = "3";
        }
        {
          name = "workspace-3-2";
          match.title = "(Saiyans Chronicles)$";
          workspace = "3";
        }
        {
          name = "workspace-4";
          match.title = "((Telegram)(.*))$";
          workspace = "4";
        }
        {
          name = "workspace-4-2";
          match.class = "(mpv)$";
          workspace = "4";
        }
        {
          name = "workspace-9";
          match.title = "(Spotify)(.*)";
          workspace = "9";
        }
      ];

      bind = [
        (exec "${mod} + CTRL + Return" "ghostty")
        (scratch "${mod} + Return" "term")
        (exec "${mod} + E" "nautilus")
        (exec "${mod} + B" "firefox")
        (exec "${mod} + SHIFT + Return" "qs ipc call launcher toggle")
        (scratch "${mod} + SHIFT + B" "btop")
        (exec "${mod} + W" "mywal -r")
        (exec "${mod} + SHIFT + P" "hyprpicker -a -f hex")
        (scratch "${mod} + I" "irssi")
        (exec "${mod} + CTRL + P" "pypr reload")

        (bind "${mod} + Q" "hl.dsp.window.close()")
        (bind "${mod} + SHIFT + Q" "hl.dsp.exit()")
        (bind "${mod} + V" ''hl.dsp.window.float({ action = "toggle" })'')
        (bind "${mod} + P" "hl.dsp.window.pseudo()")
        (bind "${mod} + F" ''hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" })'')
      ]
      ++ lib.mapAttrsToList (
        k: d: bind "${mod} + ${k}" ''hl.dsp.focus({ direction = "${d}" })''
      ) directions
      ++ lib.mapAttrsToList (
        k: d: bind "${mod} + SHIFT + ${k}" ''hl.dsp.window.move({ direction = "${d}" })''
      ) directions
      ++ lib.concatMap (
        i:
        let
          key = toString (lib.mod i 10);
          ws = toString i;
        in
        [
          (bind "${mod} + ${key}" "hl.dsp.focus({ workspace = ${ws} })")
          (bind "${mod} + SHIFT + ${key}" "hl.dsp.window.move({ workspace = ${ws} })")
        ]
      ) (lib.range 1 10)
      ++ [
        (bind "${mod} + mouse_down" ''hl.dsp.focus({ workspace = "e+1" })'')
        (bind "${mod} + mouse_up" ''hl.dsp.focus({ workspace = "e-1" })'')
        (bindOpts { mouse = true; } "${mod} + mouse:272" "hl.dsp.window.drag()")
        (bindOpts { mouse = true; } "${mod} + mouse:273" "hl.dsp.window.resize()")

        (exec "Print" ''grim -g "$(slurp)" - | wl-copy'')
        (exec "${mod} + Print" ''bash -c 'notify-send "Recording..." && wf-recorder -g "$(slurp)" -f ~/Videos/recording_$(date +%s).mp4' '')
        (exec "${mod} + SHIFT + Print" ''bash -c 'pkill -INT wf-recorder && notify-send "Recording stopped"' '')

        (exec "${mod} + Equal" "pypr zoom")
        (exec "${mod} + SHIFT + Equal" "pypr zoom +1")
        (exec "${mod} + Minus" "pypr zoom -1")

        (locked "XF86AudioRaiseVolume" "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.02+")
        (locked "XF86AudioLowerVolume" "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.02-")
        (locked "XF86AudioMute" "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
        (locked "XF86AudioPlay" "playerctl --ignore-player=firefox play-pause")
        (locked "XF86AudioNext" "playerctl --ignore-player=firefox next")
        (locked "XF86AudioPrev" "playerctl --ignore-player=firefox previous")
        (locked "${mod} + XF86AudioRaiseVolume" "wpctl set-volume @DEFAULT_AUDIO_SINK@ 1")
        (locked "${mod} + XF86AudioLowerVolume" "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.4")

      ];
    };
  };

  xdg.configFile."pypr/config.toml".source = (pkgs.formats.toml { }).generate "pypr-config.toml" {
    pyprland.plugins = [
      "scratchpads"
      "magnify"
    ];
    magnify.factor = 2;

    # alacritty, not ghostty: ghostty's single-instance activation breaks pypr's class matching
    scratchpads = {
      term = scratchpad "alacritty --class=term-scratch" // {
        class = "term-scratch";
        size = "75% 60%";
        position = "12% 4%";
        unfocus = "hide";
      };
      btop = scratchpad "alacritty --class=btop-scratch -e btop" // {
        class = "btop-scratch";
        size = "80% 80%";
        position = "10% 10%";
      };
      irssi = scratchpad "alacritty --class=irssi-scratch -e irssi" // {
        class = "irssi-scratch";
        size = "80% 80%";
        position = "10% 10%";
      };
    };
  };
}
