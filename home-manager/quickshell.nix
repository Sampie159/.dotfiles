{
  # bar, launcher and notifications; QML lives in ../quickshell
  programs.quickshell.enable = true;

  xdg.configFile.quickshell = {
    source = ../quickshell;
    recursive = true;
  };
}
