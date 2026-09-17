{ pkgs, inputs, ... }:
{
  home.packages = [
    inputs.neovim-nightly-overlay.packages.${pkgs.stdenv.hostPlatform.system}.default
    pkgs.fd # telescope find_files
    pkgs.unzip # mason.nvim LSP/DAP installs
  ];
}
