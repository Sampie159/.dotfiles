{ pkgs, inputs, ... }:
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
    withPython3 = true;
    # Nightly required - configs.lua calls require('vim._core.ui2'), which
    # errors on stable.
    package = inputs.neovim-nightly-overlay.packages.${pkgs.stdenv.hostPlatform.system}.default;
  };

  # Plugins are lazy.nvim's job (../nvim/init.lua bootstraps it, its own
  # lazy-lock.json pins versions) - Nix's job stops at getting these on PATH.
  # ~/.config/nvim itself is symlinked straight to the repo in home.nix's
  # home.file, like the rest of the app configs.
  #
  # git/ripgrep/lazygit (programs.*.enable) and a C compiler + gnumake
  # (home.nix's clang/gnumake) are already provided globally - don't
  # re-list them here, a second compiler wrapper (gcc) collides with
  # clang's in home-manager's buildEnv ("two given paths contain a
  # conflicting subpath: .../bin/cpp").
  home.packages = with pkgs; [
    fd # telescope find_files
    unzip # mason.nvim LSP/DAP installs
  ];
}
