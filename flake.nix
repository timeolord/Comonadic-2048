{
  description = "Comonadic 2048";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        compiled_package = pkgs.haskellPackages.developPackage {
          root = ./.;
        };
      in
      with pkgs;
      {
        overlays.default = (final: prev: { x2048 = compiled_package; });
        packages.default = compiled_package;
        devShells.default = mkShell {
          packages = [
            haskell-language-server
            (haskellPackages.ghcWithPackages (
              pkgs: with pkgs; [
                cabal-install
              ]
            ))
          ];
        };
      }
    );
}
