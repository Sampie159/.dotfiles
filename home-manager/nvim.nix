{ pkgs, inputs, ... }:
{
  # Not programs.neovim - even with no extraConfig/plugins set, that module
  # still generates its own init.lua (for withNodeJs/withPython3's provider
  # setup) and tries to install it through ~/.config/nvim, which home.nix
  # symlinks whole to the repo - "Error installing file '.config/nvim/init.lua'
  # outside $HOME". Just install the binary; ../nvim/init.lua (real, repo,
  # lazy.nvim-bootstrapping) is the only init file that should exist.
  #
  # Nightly required - configs.lua calls require('vim._core.ui2'), which
  # errors on stable.
  #
  # No viAlias/vimAlias either - the original Arch setup never had a vi/vim
  # alias (pacman's neovim package doesn't provide one), just `nvim`.
  #
  # git/ripgrep/lazygit (programs.*.enable) and a C compiler + gnumake
  # (home.nix's clang/gnumake) are already provided globally - don't
  # re-list them here, a second compiler wrapper (gcc) collides with
  # clang's in home-manager's buildEnv ("two given paths contain a
  # conflicting subpath: .../bin/cpp").
  home.packages = [
    inputs.neovim-nightly-overlay.packages.${pkgs.stdenv.hostPlatform.system}.default
    pkgs.fd # telescope find_files
    pkgs.unzip # mason.nvim LSP/DAP installs
  ];
}
