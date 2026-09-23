{
  programs.waybar = {
    enable = true;
    style = ./waybar.css;
    settings = [
      {
        clock = {
          format-alt = "{:%d-%m-%Y}";
          tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        };
        cpu = {
          format = "{usage}% ";
          tooltip = false;
        };
        "custom/media" = {
          escape = true;
          exec = "playerctl -p spotify -F metadata -f '{{artist}} - {{title}}' 2> /dev/null";
          format = " {}";
          max-length = 40;
        };
        height = 30;
        layer = "top";
        margin-left = 8;
        margin-right = 8;
        margin-top = 8;
        memory = {
          format = "{}% ";
        };
        modules-center = [ "hyprland/window" ];
        modules-left = [
          "hyprland/workspaces"
          "custom/media"
        ];
        modules-right = [
          "pulseaudio"
          "network"
          "cpu"
          "memory"
          "hyprland/language"
          "clock"
          "tray"
        ];
        network = {
          format-alt = "{ifname}: {cidr}";
          format-disconnected = "Offline 🔴";
          format-ethernet = "Online 🟢";
          format-linked = "{ifname} (No IP) ";
          tooltip-format = "{ifname} via {gwaddr} ";
        };
        pulseaudio = {
          format = "{volume}% {icon} {format_source}";
          format-icons = {
            default = [
              ""
              ""
              ""
            ];
            hands-free = "";
            headphone = "";
            headset = "";
          };
          format-muted = " {format_source}";
          format-source = "{volume}% ";
          format-source-muted = "";
          on-click = "pavucontrol";
        };
        spacing = 4;
        tray = {
          spacing = 10;
        };
      }
    ];
  };
}
