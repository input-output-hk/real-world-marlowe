#!/usr/bin/env bash

set -veo pipefail

mkdir -p bin

for e in cardano-cli marlowe-cli marlowe-runtime-cli InitializeRaffle ExecuteRaffle
do
  nix build .#$e
  cp -f result/bin/$e bin/$e
  rm result
done

tar cvzf raffle-bin.tar bin/
