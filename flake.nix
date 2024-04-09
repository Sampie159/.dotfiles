{
    description = "A very basic flake";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { self, nixpkgs, home-manager, ... }: 
        let 
        lib = nixpkgs.lib;
    in {
        nixosConfigurations = {
            nixos = lib.nixosSystem {
                system = "x86_64-linux";
                modules = [
                    ./configuration.nix
                    home-manager.nixosModules.home-manager {
                        home-manager = {
                            useGlobalPkgs = true;
                            useUserPackages = true;
                            users.sampie = import ./home.nix;
                        };
                    }
                ];
            };
        };
    };
}
