{
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

        rust-overlay = {
            url = "github:oxalica/rust-overlay";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { nixpkgs, rust-overlay, ... }:
        let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
                inherit system;
                overlays = [ rust-overlay.overlays.default ];
            };
        in
        {
            devShells.${system}.default = pkgs.mkShell {
                packages = [
                    (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)
                    pkgs.pkg-config
                ];
                # -sys crate deps go here, e.g. pkgs.openssl
                buildInputs = [ ];
            };
        };
}
