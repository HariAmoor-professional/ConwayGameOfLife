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
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, flake-root, haskellNix, treefmt-nix }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        flake-parts.flakeModules.easyOverlay
        treefmt-nix.flakeModule
        flake-root.flakeModule
      ];
      systems = nixpkgs.lib.systems.flakeExposed;

      perSystem = { config, pkgs, system, self', ... }: {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay ];
        };

        treefmt.config = with pkgs; {
          inherit (config.flake-root) projectRootFile;
          package = treefmt;

          programs = {
            ormolu = {
              enable = true;
              package = haskellPackages.fourmolu;
            };
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            hlint.enable = true;
          };

          settings.formatter.ormolu.options = [
            "--ghc-opt"
            "-XImportQualifiedPost"
          ];
        };

        packages.default = ((pkgs.haskell-nix.project' {
          name = "conway-game-of-life";
          src = ./.;
          evalSystem = system;

          compiler-nix-name = "ghc926"; # Version of GHC to use

          shell = {
            tools = {
              cabal = "latest";
              hlint = "latest";
              haskell-language-server = "latest";
            };

            buildInputs = with pkgs; [ nixpkgs-fmt ];
          };
        }).flake { }).packages."conway-game-of-life:exe:life";
        devShells.default = (self'.packages.default.flake { }).devShells.default;
      };
    };

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    "extra-substituters" = [ "https://cache.iog.io" ];
    "extra-trusted-public-keys" = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    "allow-import-from-derivation" = "true";
  };
}
