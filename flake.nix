{
    description = "sampie's NixOS + home-manager config";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        zig = {
            url = "github:mitchellh/zig-overlay";
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

    outputs = inputs @ { nixpkgs, home-manager, chaotic, nixos-hardware, ... }: {
        nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit inputs; };
            modules = [
                ./configuration.nix
                chaotic.nixosModules.default
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
    };
}
