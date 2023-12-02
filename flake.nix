{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "nixpkgs-unstable"; # primary nixpkgs
    zig.url = "github:mitchellh/zig-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, zig, ... }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ zig.overlays.default ];
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            zigpkgs.master
            haskell.compiler.ghc96
            haskell.packages.ghc96.haskell-language-server
          ];
        };
      }
    );
}
