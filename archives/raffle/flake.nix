# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{

  description = "Marlowe Raffle Example";

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };

  inputs = {
    std = {
      url = "github:divnix/std";
      inputs.n2c.follows = "n2c";
    };
    n2c.url = "github:shlevy/nix2container/no-Size-on-dir";
    iogx.url = "github:input-output-hk/iogx";
    marlowe = {
      type = "github";
      owner = "input-output-hk";
      repo = "marlowe-cardano";
      ref = "f49df7c0490ebda390154fd90940f94b430b99b4";
    };
    cardano-world.follows = "marlowe/cardano-world";
  };

  outputs = inputs: inputs.iogx.lib.mkFlake inputs ./.;

}
