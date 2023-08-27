{ inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-23.05;

    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = pkgsNew: pkgsOld: {
          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides = pkgsNew.haskell.lib.packageSourceOverrides {
              composable-path = ./.;
            };
          });
        };

        pkgs =
          import nixpkgs { inherit config system; overlays = [ overlay ]; };

      in
        rec {
          packages.default = pkgs.haskellPackages.composable-path;

          devShells.default = pkgs.haskellPackages.composable-path.env;
        }
    );
}
