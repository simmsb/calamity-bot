{
  description = "calamity-bot";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    gitignore.url = "github:hercules-ci/gitignore.nix";
    gitignore.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";

    haskell-flake.url = "github:srid/haskell-flake";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    flake-root.url = "github:srid/flake-root";

    calamity.url = "github:simmsb/calamity";
    calamity.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs-140774-workaround.url = "github:srid/nixpkgs-140774-workaround";

    all-cabal-hashes.url = "github:commercialhaskell/all-cabal-hashes/hackage";
    all-cabal-hashes.flake = false;

    nix2container.url = "github:nlewo/nix2container";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
  };


  outputs = inputs@{ self, nixpkgs, gitignore, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      # systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          imports = [
            inputs.nixpkgs-140774-workaround.haskellFlakeProjectModules.default
          ];

          basePackages = pkgs.haskell.packages.ghc944.override {
            all-cabal-hashes = inputs.all-cabal-hashes;
            overrides = (final: prev: { ormolu = final.ormolu_0_5_3_0; });
          };

          packages = {
            calamity-bot.root = lib.cleanSourceWith {
              filter = name: type: let baseName = baseNameOf (toString name); in !(lib.hasPrefix "flake" baseName);
              src = lib.cleanSource ./.;
            };
          };

          devShell = {
            tools = hp:
              {
                ghcid = hp.ghcid;
                treefmt = config.treefmt.build.wrapper;
              } // config.treefmt.build.programs;
            hlsCheck.enable = false;
          };

          overrides = self: super: with pkgs.haskell.lib; {
            ghcid = dontCheck super.ghcid;

            calamity = inputs.calamity.packages.${system}.calamity;
            calamity-commands = inputs.calamity.packages.${system}.calamity-commands;

            ListLike = dontCheck super.ListLike;
            type-errors = dontCheck (super.callHackage "type-errors" "0.2.0.1" { });
            polysemy-plugin = dontCheck (super.callHackage "polysemy-plugin" "0.4.4.0" { });
            polysemy = dontCheck (super.callHackage "polysemy" "1.9.0.0" { });
            typerep-map = dontCheck (super.callHackage "typerep-map" "0.6.0.0" { });
            aeson-optics = dontCheck (super.callHackage "aeson-optics" "1.2.0.1" { });
            di-core = dontCheck super.di-core;
            optics = dontCheck super.optics;

            http-api-data = dontCheck (super.callHackage "http-api-data" "0.5" { });
            # this v is needed so http-api-data builds
            attoparsec-iso8601 = dontCheck (super.callHackage "attoparsec-iso8601" "1.1.0.0" { });

            universum = dontCheck (super.callHackage "universum" "1.8.1.1" { });
            arbor-lru-cache = dontCheck (super.callHackage "arbor-lru-cache" "0.1.1.1" { });
            resource-pool = dontCheck (self.callHackage "resource-pool" "0.3.1.0" { });
            beam-core = dontCheck (self.callHackage "beam-core" "0.10.0.0" { });
            postgresql-simple = dontCheck (self.callHackage "postgresql-simple" "0.6.5" { });
            beam-postgres = dontCheck (self.callHackage "beam-postgres" "0.5.3.0" { });
            beam-migrate = dontCheck (self.callHackage "beam-migrate" "0.5.2.0" { });
            prometheus = dontCheck (doJailbreak (self.callHackage "prometheus" "2.2.3" { }));
          };
        };

        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        packages.default = self'.packages.calamity-bot;
        packages.stripped = pkgs.haskell.lib.justStaticExecutables self'.packages.calamity-bot;
        packages.ociImage =
          let
            sysdeps = pkgs.buildEnv {
              name = "calamity-bot-deps";
              paths = with pkgs; [
                bashInteractive
                busybox
                cacert
                ffmpeg
                (texlive.combine {
                  inherit (texlive) scheme-medium standalone preview was;
                })
                zlib.dev
                zlib.out
                librsvg
                gmp
              ];
              pathsToLink = [ "/bin" "/etc" ];
            };
          in
          inputs.nix2container.packages.${system}.nix2container.buildImage {
            name = "ghcr.io/simmsb/calamity-bot";
            tag = "latest";
            config = {
               entrypoint = [ "${self'.packages.stripped}/bin/calamity-bot" "+RTS" "-I0" "-RTS" ];
            };
            layers = [ (inputs.nix2container.packages.${system}.nix2container.buildLayer { deps = [ sysdeps ]; }) ];
            copyToRoot = sysdeps;
          };
      };
    };
}
