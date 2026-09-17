{
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

        zig = {
            url = "github:mitchellh/zig-overlay";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { nixpkgs, zig, ... }:
        let
            system = "x86_64-linux";
            pkgs = nixpkgs.legacyPackages.${system};
        in
        {
            devShells.${system}.default = pkgs.mkShell {
                # or a release, e.g. zig.packages.${system}."0.15.1"
                packages = [ zig.packages.${system}.master ];
            };
        };
}
