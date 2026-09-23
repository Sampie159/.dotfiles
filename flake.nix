{
  description = "sampie's NixOS + home-manager config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    odin-mode = {
      url = "github:Sampie159/odin-mode";
      flake = false;
    };

    slang-mode = {
      url = "github:K1ngst0m/slang-mode";
      flake = false;
    };

    vimplugin-c3 = {
      url = "github:Sampie159/c3.vim";
      flake = false;
    };

    vimplugin-compile-mode = {
      url = "github:ej-shafran/compile-mode.nvim";
      flake = false;
    };

    vimplugin-darkvoid = {
      url = "github:aliqyan-21/darkvoid.nvim";
      flake = false;
    };

    vimplugin-dookie = {
      url = "github:pebeto/dookie.nvim";
      flake = false;
    };

    vimplugin-everblush = {
      url = "github:Everblush/nvim";
      flake = false;
    };

    vimplugin-evergarden = {
      url = "github:everviolet/nvim";
      flake = false;
    };

    vimplugin-flow = {
      url = "github:0xstepit/flow.nvim";
      flake = false;
    };

    vimplugin-fogbell = {
      url = "github:Sampie159/fogbell.vim";
      flake = false;
    };

    vimplugin-jellybeans = {
      url = "github:wtfox/jellybeans.nvim";
      flake = false;
    };

    vimplugin-komau = {
      url = "github:ntk148v/komau.vim";
      flake = false;
    };

    vimplugin-mellifluous = {
      url = "github:ramojus/mellifluous.nvim";
      flake = false;
    };

    vimplugin-mfd = {
      url = "github:kungfusheep/mfd.nvim";
      flake = false;
    };

    vimplugin-mies = {
      url = "github:jaredgorski/Mies.vim";
      flake = false;
    };

    vimplugin-minimal = {
      url = "github:Yazeed1s/minimal.nvim";
      flake = false;
    };

    vimplugin-modern-borland = {
      url = "github:letorbi/vim-colors-modern-borland";
      flake = false;
    };

    vimplugin-neogotham = {
      url = "gitlab:shmerl/neogotham";
      flake = false;
    };

    vimplugin-oldworld = {
      url = "github:dgox16/oldworld.nvim";
      flake = false;
    };

    vimplugin-ouroboros = {
      url = "github:Sampie159/ouroboros.nvim";
      flake = false;
    };

    vimplugin-sqls = {
      url = "github:nanotee/sqls.nvim";
      flake = false;
    };

    vimplugin-sweetie = {
      url = "github:NTBBloodbath/sweetie.nvim";
      flake = false;
    };

    vimplugin-thorn = {
      url = "github:jpwol/thorn.nvim";
      flake = false;
    };

    vimplugin-todo-comments = {
      url = "github:travisvroman/todo-comments.nvim";
      flake = false;
    };

    vimplugin-uiua = {
      url = "github:Apeiros-46B/uiua.vim";
      flake = false;
    };

    vimplugin-vacme = {
      url = "github:raphael-proust/vacme";
      flake = false;
    };

    vimplugin-vesper = {
      url = "github:datsfilipe/vesper.nvim";
      flake = false;
    };
  };

  outputs =
    inputs@{
      nixpkgs,
      home-manager,
      chaotic,
      nixos-hardware,
      ...
    }:
    let
      lib = nixpkgs.lib;
    in
    {
      nixosConfigurations.nixos = lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./configuration.nix
          chaotic.nixosModules.default
          { chaotic.mesa-git.enable = true; }
          nixos-hardware.nixosModules.common-cpu-amd
          nixos-hardware.nixosModules.common-gpu-amd
          nixos-hardware.nixosModules.common-pc-ssd
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              backupFileExtension = "hm-bak";
              extraSpecialArgs = { inherit inputs; };
              users.sampie = import ./home.nix;
            };
          }
        ];
      };
    };
}
