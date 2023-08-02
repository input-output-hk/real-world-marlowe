{
  description = "Marlowe Starter Kit";

  nixConfig = {
    extra-substituters = [
      "https://cache.zw3rk.com"
      "https://cache.iog.io"
      "https://hydra.iohk.io"
      "https://tweag-jupyter.cachix.org"
    ];
    extra-trusted-public-keys = [
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "tweag-jupyter.cachix.org-1:UtNH4Zs6hVUFpFBTLaA4ejYavPo5EFFqgd7G7FxGW9g="
    ];
  };

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    marlowe = {
      type = "github";
      owner = "input-output-hk";
      repo = "marlowe-cardano";
      ref = "f49df7c0490ebda390154fd90940f94b430b99b4";
    };
    cardano-world.follows = "marlowe/cardano-world";
  };

  outputs = { self, flake-compat, flake-utils, nixpkgs, marlowe, cardano-world }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        cp = cardano-world.${system}.cardano.packages;
        mp = marlowe.packages.${system};
        ghcWithPackages = pkgs.haskell.packages.ghc8107.ghcWithPackages (p: [
          p.aeson
          p.bytestring
          p.MissingH
          p.shh
          p.PyF
        ]);
        extraPackages = p: [
          mp.marlowe-cli-exe-marlowe-cli-ghc8107
          mp.marlowe-runtime-cli-exe-marlowe-runtime-cli-ghc8107
          cp.cardano-cli
          p.z3
          p.coreutils
          p.curl
          p.gnused
          p.gnugrep
          p.jq
          p.json2yaml
          p.yaml2json
          ghcWithPackages
          p.gcc
          p.cabal-install
        ];
      in rec {
        devShell = pkgs.mkShell {
          buildInputs = extraPackages pkgs;
        };
      }
    );
}
