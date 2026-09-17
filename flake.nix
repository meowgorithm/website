{
  description = "";

  inputs = {
    # nixpkgs-unstable, locked to a specific revision in flake.lock.
    # Pinned to unstable (not a stable release branch) because stable
    # nixos-25.05 only ships GHC 9.10.1/9.10.2, while the Dockerfile
    # builds with GHC 9.10.3.
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      ghcVersion = "ghc9103";

      pkgs = nixpkgs.legacyPackages.${system};

      haskellPackages = pkgs.haskell.packages.${ghcVersion}.override {
        overrides = self: super: {
          scotty = pkgs.haskell.lib.dontCheck (self.callHackage "scotty" "0.30" {});
        };
      };

      static = pkgs.buildNpmPackage {
        name = "static";
        src = ./.;
        npmDepsHash = "sha256-L6XHdb1jyNlPhFph79yL3KIdaOpilTBOcyqyAvdao4Y="; # regenerate with: replace with pkgs.lib.fakeHash and read the hash from the build error
        installPhase = ''
          mkdir $out
          cp -r static/ $out
        '';
      };

      webserver = haskellPackages.callCabal2nix "rocha" ./. {};
    in {
      packages = {
        static = static;
        default = let
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
      };

      devShells.default = haskellPackages.shellFor {
        packages = p: [];
        buildInputs = with pkgs; [
          haskellPackages.blaze-from-html
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.zlib
          nodejs_22
          zlib
        ];
      };
    });
}
