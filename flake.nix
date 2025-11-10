{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs-prev.url = "github:nixos/nixpkgs?rev=d31a91c9b3bee464d054633d5f8b84e17a637862"; # 2025-07-01
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nixpkgs-prev,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {system = system;};
        pkgs-prev = import nixpkgs-prev {system = system;};
      in {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs-prev.comby
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-json
            pkgs.just
            pkgs.nodejs-slim_24
            pkgs.nushell
            pkgs.opencc
            pkgs.pnpm
            pkgs.qrtool
          ];
        };
      }
    );
}
