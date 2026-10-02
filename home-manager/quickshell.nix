{ link, ... }:
{
  programs.quickshell.enable = true;

  xdg.configFile.quickshell.source = link "quickshell";
}
