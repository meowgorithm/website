{
  description = "";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      ghcVersion = "ghc947";

      pkgs = nixpkgs.legacyPackages.${system};

      haskellPackages = pkgs.haskell.packages.${ghcVersion}.override {
        overrides = self: super: {
          scotty = pkgs.haskell.lib.dontCheck (self.callHackage "scotty" "0.20.1" {});
        };
      };

      static = pkgs.buildNpmPackage {
        name = "static";
        src = ./.;
        npmDepsHash = "sha256-Yu8AZGsEryACdrQC1bq/LhBBCANhEMnCV6hCWggU/KI=";
        installPhase = ''
          mkdir $out
          cp -r static/ $out
        '';
      };

      webserver = haskellPackages.callCabal2nix "rocha" ./. {};
    in {
      defaultPackage = let
        name = "website";
      in
        pkgs.stdenv.mkDerivation {
          name = name;
          phases = ["installPhase"];
          buildInputs = [webserver static];
          installPhase = ''
            mkdir -p $out/bin
            cp -r ${webserver}/bin/webserver $out
            cp -r ${static}/static $out
            ln -s ${webserver}/bin/webserver $out/bin/${name} # for 'nix run'
          '';
        };

      devShell = haskellPackages.shellFor {
        packages = p: [];
        buildInputs = with pkgs; [
          haskellPackages.blaze-from-html
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.zlib
          nodejs_18
          zlib
        ];
      };
    });
}
