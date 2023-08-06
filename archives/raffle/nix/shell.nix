# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

{ inputs, inputs', pkgs, project }:

{
  name = "raffle";
  packages = [
    inputs.cardano-world.cardano.packages.cardano-address
    inputs.cardano-world.cardano.packages.cardano-node
    inputs.cardano-world.cardano.packages.cardano-cli
    inputs.marlowe.packages.marlowe-cli-exe-marlowe-cli-ghc8107
    inputs.marlowe.packages.marlowe-runtime-cli-exe-marlowe-runtime-cli-ghc8107
    pkgs.z3
    pkgs.coreutils
    pkgs.curl
    pkgs.gnused
    pkgs.gnugrep
    pkgs.jq
    pkgs.json2yaml
    pkgs.yaml2json
  ];
}
