# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{
  inputs, inputs', projects, pkgs
} :

{
  packages = {
    cardano-cli = inputs.cardano-world.cardano.packages.project.projectCross.musl64.hsPkgs.cardano-cli.components.exes.cardano-cli;
    marlowe-cli = inputs.marlowe.packages.marlowe-cli-exe-marlowe-cli-ghc8107.project.projectCross.musl64.hsPkgs.marlowe-cli.components.exes.marlowe-cli;
    marlowe-runtime-cli = inputs.marlowe.packages.marlowe-runtime-cli-exe-marlowe-runtime-cli-ghc8107.project.projectCross.musl64.hsPkgs.marlowe-runtime-cli.components.exes.marlowe-runtime-cli;
    InitializeRaffle = projects.ghc8107.projectCross.musl64.hsPkgs.raffle.components.exes.InitializeRaffle;
    ExecuteRaffle = projects.ghc8107.projectCross.musl64.hsPkgs.raffle.components.exes.ExecuteRaffle;
  };
}
