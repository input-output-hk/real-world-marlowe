# Marlowe Raffle


## Instructions for Running Raffle


### 1. Enter a Nix development shell

Executing the `nix develop` command in this folder will open a shell with all of the tools required for the raffle.


### 2. Create a configuration file

Copy the file [inputs/configuration.json](inputs/configuration.json) and edit the following:

1. The deadlines.
2. The URI for Marlowe Runtime.
3. The path to the address of the raffle's sponsor.
4. The path to the signing key for the raffle's sponsor.


### 3. Set the eligible addresses for the prize

1. Create a file like [inputs/prizes.json](inputs/prizes.json) that contains one entry for the role token controlling the prize that is being awarded.
2. Create a file like [inputs/parties1.json](inputs/parties1.json) that contains entries for the eligible addresses for the raffle prize.


### 4. Initialize the Marlowe contract for raffle


The following command builds the contract and submits its creation transaction to the blockchain.

```bash
./InitializeRaffle.hs configuration.json addresses.json prizes.json
```

Store the contract ID in the shell variable `$CONTRACT_ID`.


### 5. Deposit the prize token and run the raffle

The following command deposits the prize token in the contract and runs the raffle.

```bash
./ExecuteRaffle.hs configuration.json addresses.json prizes.json "$CONTRACT_ID"
```

The Marlowe contract sends the prize token to the winner's address. They redeem the prize at Marlowe's role-payout validator.


### 6. Additional prizes

Reconfigure the eligible addresses, removing the previous winner, and prize token in step 3 above and repeat steps 4 and 5 for each additional prize.


## Instructions for Building Raffle executables

On a Nix system, run the command `./build.sh` to compile static Linux executables of all of the tools and packed them in a file `raffle-bin.tar`.
