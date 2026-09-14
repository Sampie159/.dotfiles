{ pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
  };

  # Packages are elpaca's job (../emacs/init.el bootstraps it, packages.el's
  # use-package/:ensure forms - including odin-mode/slang-mode via :host
  # github recipes - pin themselves same as any non-nix Emacs setup).
  # ~/.config/emacs is symlinked straight to the repo in home.nix's
  # home.file, like the rest of the app configs.
  #
  # git/ripgrep (programs.*.enable) and a C compiler + cmake + libtool for
  # vterm's native module (home.nix's clang/cmake/libtool) are already
  # provided globally - don't re-list them here, nothing left to add.
}
