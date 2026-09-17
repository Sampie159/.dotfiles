{ pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    # vterm's native module is built by nix; every other package is elpaca's (../emacs/packages.el)
    extraPackages = epkgs: [ epkgs.vterm ];
  };
}
