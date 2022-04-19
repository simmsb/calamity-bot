{
  description = "calamity-bot";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;
  inputs.flake-compat.inputs.nixpkgs.follows = "nixpkgs";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nix2container.url = "github:nlewo/nix2container";

  outputs = { self, nixpkgs, flake-utils, flake-compat, haskellNix, gitignore, nix2container }:
    let inherit (gitignore.lib) gitignoreSource;
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        bot-overlay = (final: prev: {
          calamity-bot =
            final.haskell-nix.project' {
              src = gitignoreSource ./.;
              compiler-nix-name = "ghc8107";
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
                fourmolu = {
                  modules = [
                    ({ lib, ... }: {
                      options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "Cabal"; };
                    })
                  ];
                };
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                haskellPackages.implicit-hie
                haskellPackages.cabal-fmt
              ];
            };
        });

        libm-overlay = self: _: {
          m = self.stdenv.mkDerivation {
            name = "m";
            unpackPhase = "true";
            installPhase = "mkdir -p $out";
          };
        };

        overlays = [
          haskellNix.overlay
          bot-overlay
          libm-overlay
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        nix2containerPkgs = nix2container.packages.${system};
        flake = pkgs.calamity-bot.flake { };
        tex = pkgs.texlive.combine {
          inherit (pkgs.texlive) scheme-medium standalone preview was;
        };
      in
      flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."calamity-bot:exe:calamity-bot";

        packages = {
          ociImage =
            let bot = self.defaultPackage.${system}; in
            nix2containerPkgs.nix2container.buildImage {
              name = "ghcr.io/simmsb/calamity-bot";
              tag = "latest";
              layers = [
                (nix2containerPkgs.nix2container.buildLayer { deps = [
                  pkgs.bashInteractive
                  pkgs.busybox
                  pkgs.cacert
                  pkgs.ffmpeg
                  pkgs.zlib.dev
                  pkgs.zlib.out
                  pkgs.librsvg
                  pkgs.gmp
                ]; })
                (nix2containerPkgs.nix2container.buildLayer { deps = [
                  tex
                ]; })
                (nix2containerPkgs.nix2container.buildLayer { deps = [
                  bot
                ]; })
              ];
              config = {
                cmd = [ "/bin/calamity-bot" ];
              };
            };
        };
      });
}
