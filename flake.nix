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
  outputs = { self, nixpkgs, flake-utils, flake-compat, haskellNix, gitignore }:
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
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                haskellPackages.implicit-hie
                haskellPackages.cabal-fmt
                haskellPackages.fourmolu
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
        flake = pkgs.calamity-bot.flake { };
      in
      flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."calamity-bot:exe:calamity-bot";

        packages = {
          ociImage =
            let bot = self.defaultPackage.${system}; in
            pkgs.dockerTools.buildLayeredImage {
              name = bot.name;
              tag = "latest";
              contents = [ bot pkgs.bashInteractive pkgs.busybox pkgs.cacert ];
              config = {
                Cmd = [ "/bin/calamity-bot" ];
              };
              created = "now";
              maxLayers = 120;
            };
        };
      });
}
