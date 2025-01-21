{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };

        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        hls = pkgs.haskell-language-server.override {
          supportedGhcVersions = [ "98" ];
        };

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in rec {

        packages = let
          makeTestConfiguration = { ghcVersion, overrides ? new: old: { } }:
            let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
            in (pkgs.haskell.packages.${ghcVersion}.override (old: {
              overrides = combineOverrides old [
                (packageSourceOverrides { unfork = ./unfork; })
                overrides
              ];
            })).unfork;
        in rec {
          ghc-9-6 = makeTestConfiguration { ghcVersion = "ghc96"; };
          ghc-9-8 = makeTestConfiguration { ghcVersion = "ghc98"; };
          all = pkgs.symlinkJoin {
            name = "unfork-tests";
            paths = [ ghc-9-6 ghc-9-8 ];
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ packages.ghc-9-8.env ];
          buildInputs = [ pkgs.cabal-install ];
        };

      });
}
