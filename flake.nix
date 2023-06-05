{
  # This is a template created by `hix init`
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-root.url = "github:srid/flake-root";
    flake-parts.url = "github:hercules-ci/flake-parts";
    hls.url = "github:haskell/haskell-language-server";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, flake-root, haskellNix, treefmt-nix, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        flake-parts.flakeModules.easyOverlay
        treefmt-nix.flakeModule
        flake-root.flakeModule
      ];
      systems = nixpkgs.lib.systems.flakeExposed;

      perSystem = { self', inputs', system, lib, config, pkgs, ... }:
        let
          compiler-version = "961";
          compiler-nix-name = "ghc${compiler-version}";
          projectFlake = with pkgs; (haskell-nix.project' {
            inherit compiler-nix-name;

            name = "conway-game-of-life";
            src = ./.;
            evalSystem = system;

            shell = {
              tools.cabal = "latest";

              buildInputs = [
                # Needs `haskell-language-server-wrapper` for editor
                inputs'.hls.packages."haskell-language-server-${compiler-version}"
              ];
            };
          }).flake { };
        in
        {
          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = [ haskellNix.overlay ];
          };

          treefmt.config = with pkgs; {
            inherit (config.flake-root) projectRootFile;
            package = treefmt;

            programs = {
              # ormolu = {
              # enable = true;
              # package = haskellPackages.fourmolu;
              # };
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              cabal-fmt.enable = true;
              hlint.enable = true;
            };

            settings.formatter.ormolu.options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };

          packages.default = projectFlake.packages."conway-game-of-life:exe:life";
          devShells.default = projectFlake.devShells.default;
        };
    };

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
