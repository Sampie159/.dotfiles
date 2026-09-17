{
    inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    outputs = { nixpkgs, ... }:
        let
            system = "x86_64-linux";
            pkgs = nixpkgs.legacyPackages.${system};
        in
        {
            devShells.${system}.default = pkgs.mkShell.override { stdenv = pkgs.clangStdenv; } {
                packages = with pkgs; [ cmake meson ninja pkg-config ];
                # libraries go here so pkg-config/cmake can find them
                buildInputs = with pkgs; [ ];
                CMAKE_EXPORT_COMPILE_COMMANDS = "1";
            };
        };
}
