{ pkgs, link, ... }:
{
  home.packages = [ pkgs.hyprpicker ];

  # HM keeps hyprland.lua for the systemd session hooks; everything else lives in ../hypr/main.lua
  wayland.windowManager.hyprland = {
    enable = true;
    package = null;
    portalPackage = null;
    extraConfig = ''require("main")'';
  };

  xdg.configFile."hypr/main.lua".source = link "hypr/main.lua";
  xdg.configFile."pypr/config.toml".source = link "pypr/config.toml";
}
