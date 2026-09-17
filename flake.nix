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
    };

    outputs = inputs @ { nixpkgs, home-manager, chaotic, nixos-hardware, ... }:
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
                            # first activation: move pre-existing dotfiles aside
                            # instead of aborting the rebuild
                            backupFileExtension = "hm-bak";
                            extraSpecialArgs = { inherit inputs; };
                            users.sampie = import ./home.nix;
                        };
                    }
                ];
            };

            # nix flake init -t ~/.dotfiles#<name>
            templates = rec {
                c = { path = ./flake-templates/c; description = "C/C++ dev shell (clang, cmake, meson)"; };
                zig = { path = ./flake-templates/zig; description = "Zig dev shell (zig-overlay)"; };
                rust = { path = ./flake-templates/rust; description = "Rust dev shell (rust-overlay + rust-toolchain.toml)"; };
                default = c;
            };
        };
}
