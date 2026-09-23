{
  lib,
  pkgs,
  inputs,
  ...
}:
let
  # every `vimplugin-<name>` flake input becomes a plugin, pinned by flake.lock
  fromInputs = lib.mapAttrsToList (
    name: src:
    pkgs.vimUtils.buildVimPlugin {
      pname = lib.removePrefix "vimplugin-" name;
      version = "0-unstable";
      inherit src;
      doCheck = false;
    }
  ) (lib.filterAttrs (name: _: lib.hasPrefix "vimplugin-" name) inputs);
in
{
  programs.neovim = {
    enable = true;
    package = inputs.neovim-nightly-overlay.packages.${pkgs.stdenv.hostPlatform.system}.default;
    # init.lua comes from the repo, keep HM's generated bits out of it
    sideloadInitLua = true;

    plugins =
      with pkgs.vimPlugins;
      [
        baleia-nvim
        boo-colorscheme-nvim
        catppuccin-nvim
        cmp-buffer
        cmp_luasnip
        cmp-nvim-lsp
        comment-nvim
        deepwhite-nvim
        diffview-nvim
        elixir-tools-nvim
        everforest
        fidget-nvim
        git-conflict-nvim
        gitsigns-nvim
        gruvbox-baby
        gruvbox-material
        harpoon2
        kanagawa-nvim
        lazydev-nvim
        lazygit-nvim
        lsp_signature-nvim
        lualine-nvim
        luasnip
        lush-nvim
        melange-nvim
        miasma-nvim
        modus-themes-nvim
        neogit
        nightfly
        nightfox-nvim
        night-owl-nvim
        nvim-cmp
        nvim-lspconfig
        nvim-parinfer
        nvim-spectre
        nvim-treesitter.withAllGrammars
        nvim-web-devicons
        oil-nvim
        oxocarbon-nvim
        plenary-nvim
        poimandres-nvim
        quickmath-nvim
        telescope-fzf-native-nvim
        telescope-nvim
        telescope-ui-select-nvim
        text-case-nvim
        tokyonight-nvim
        undotree
        vim-dadbod
        which-key-nvim
        yuck-vim
        zenbones-nvim
      ]
      ++ fromInputs;

    extraPackages = with pkgs; [
      fd # telescope find_files
      lua-language-server
    ];
  };

  xdg.configFile.nvim = {
    source = ../nvim;
    recursive = true;
  };
}
