{
  description = "calamity-bot";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.hls.url = "github:haskell/haskell-language-server";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.calamity.url = "github:simmsb/calamity";

  outputs = { self, nixpkgs, flake-utils, hls, gitignore, calamity }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        main-sources = gitignore.lib.gitignoreSource ./.;
      in
      with pkgs;
      let
        botBuilder = hPkgs:
          let
            shell = pkg.env.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ [ cabal-install zlib ];
            });

            # Shell with haskell language server
            shell_hls = shell.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ [ hPkgs.haskell-language-server ];
            });


            pkg = (haskell.lib.buildFromSdist
              (hPkgs.callCabal2nix "calamity-bot" main-sources { })).overrideAttrs
              (oldAttrs: {
                buildInputs = oldAttrs.buildInputs;
                passthru = oldAttrs.passthru // { inherit shell shell_hls; };
              });
            # Add the GHC version in the package name
          in
          pkg.overrideAttrs (old: { name = "calamity-bot-ghc${hPkgs.ghc.version}"; });
      in
      rec {
        packages = rec {
          calamity_bot =
            with haskell.lib; let
              hPkgs = haskell.packages.ghc923.override {
                overrides = self: super: {
                  aeson-optics = dontCheck (self.callHackage "aeson-optics" "1.2" { });
                  type-errors = dontCheck (self.callHackage "type-errors" "0.2.0.0" { });
                  polysemy-plugin = dontCheck (self.callHackage "polysemy-plugin" "0.4.3.1" { });
                  polysemy = dontCheck (self.callHackage "polysemy" "1.7.1.0" { });
                  text = dontCheck (self.callHackage "text" "2.0.1" { });
                  parsec = dontCheck (self.callHackage "parsec" "3.1.15.1" { });
                  conduit-extra = dontCheck (self.callHackage "conduit-extra" "1.3.6" { });
                };
              };
            in

            botBuilder (hPkgs.override
              (old: {
                overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { })) (self: super: {
                  arbor-lru-cache = dontCheck (super.callHackage "arbor-lru-cache" "0.1.1.1" { });
                  calamity = calamity.lib.${system}.calamityPkg hPkgs;
                  calamity-commands = calamity.lib.${system}.calamityCommandsPkg hPkgs;
                  relude = dontCheck (super.callHackage "relude" "1.1.0.0" { });
                  resource-pool = dontCheck (self.callHackage "resource-pool" "0.3.1.0" { });
                  beam-core = dontCheck (doJailbreak (self.callHackage "beam-core" "0.9.2.1" { }));
                  beam-postgres = dontCheck (doJailbreak (self.callHackage "beam-postgres" "0.5.2.1" { }));
                  beam-migrate = dontCheck (doJailbreak (self.callHackage "beam-migrate" "0.5.1.2" { }));
                  prometheus = dontCheck (doJailbreak (self.callHackage "prometheus" "2.2.3" { }));
                });
              }));

          default = calamity_bot;
          devShells.default = calamity_bot.shell_hls;

          bot_static = haskell.lib.justStaticExecutables calamity_bot;

          ociImage =
            let
              tex = pkgs.texlive.combine {
                inherit (pkgs.texlive) scheme-medium standalone preview was;
              };
            in
            pkgs.dockerTools.buildLayeredImage {
              name = "ghcr.io/simmsb/calamity-bot";
              tag = "latest";
              contents = [
                bot_static
                pkgs.bashInteractive
                pkgs.busybox
                pkgs.cacert
                pkgs.ffmpeg
                tex
                pkgs.zlib.dev
                pkgs.zlib.out
                pkgs.librsvg
                pkgs.gmp
              ];
              config = {
                Cmd = [ "${bot_static}/bin/calamity-bot" "+RTS" "-I0" "-RTS" ];
              };
              created = "now";
            };
        };
      });
}
