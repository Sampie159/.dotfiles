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

    outputs = inputs @ { nixpkgs, home-manager, chaotic, nixos-hardware, ... }:
        let
            lib = nixpkgs.lib;

            # isVM is threaded into configuration.nix as a specialArg to gate the
            # handful of settings that only make sense on real hardware (cachyos
            # kernel, RDNA4 mesa-git). chaotic.mesa-git.enable can't just be
            # `lib.mkIf (!isVM)`'d from inside configuration.nix - the option
            # doesn't exist at all unless chaotic.nixosModules.default is
            # imported, so that whole module (and its one setting) is only
            # appended for the real-hardware host.
            mkHost = { isVM }: lib.nixosSystem {
                system = "x86_64-linux";
                specialArgs = { inherit inputs isVM; };
                modules = [
                    ./configuration.nix
                ]
                ++ lib.optionals (!isVM) [
                    chaotic.nixosModules.default
                    { chaotic.mesa-git.enable = true; }
                ]
                ++ [
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
        in
        {
            # Real hardware: RX 9070 XT (RDNA4) via Chaotic Nyx mesa-git, cachyos kernel.
            nixosConfigurations.nixos = mkHost { isVM = false; };

            # `nixos-rebuild ... --flake .#vm`: QEMU test build - no RDNA4 GPU, so
            # chaotic/mesa-git is skipped (also skips its slow fetch), stock
            # kernel instead of cachyos.
            nixosConfigurations.vm = mkHost { isVM = true; };
        };
}
