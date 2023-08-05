
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

import Data.Maybe
import GHC.Generics
import Shh
import Shh.Internal (toArgs)
import System.Environment (getArgs)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString.Lazy.Char8 as C

cat :: Command a => a
cat = toArgs ["cat"]

cardano_cli :: Command a => a
cardano_cli = toArgs ["cardano-cli"]

marlowe_cli :: Command a => a
marlowe_cli = toArgs ["marlowe-cli"]

marlowe_runtime_cli :: Command a => a
marlowe_runtime_cli = toArgs ["marlowe-runtime-cli"]

echo :: Command a => a
echo = toArgs ["echo"]

type PolicyId = String

type TokenName = String

type ContractId = String

type AddressBech32 = String

data RaffleConfiguration 
    = RaffleConfiguration 
    { runtimeURI :: RuntimeURI
    , contract :: FilePath -- where to save the raffle contract json generated
    , state :: FilePath -- where to save the raffle contract state generated
    , chunkSize :: Integer -- parameter for generatiing the contract
    , tmpTxToSign :: FilePath -- tmp file for tx created to sign
    , tmpTxToSubmit :: FilePath -- tmp file for signed tx to submit
    , sponsorAddressFilePath :: FilePath
    , sponsorPrivateKeyFilePath :: FilePath
    , deadlines :: Deadlines
    } deriving (Show,Generic,A.FromJSON,A.ToJSON)

newtype Sponsor = Sponsor {s_address :: String} deriving (Show,Generic,A.FromJSON,A.ToJSON)

newtype Oracle = Oracle {o_address :: String} deriving (Show,Generic,A.FromJSON,A.ToJSON)

data RuntimeURI = RuntimeURI {host :: String, proxy_port :: Integer, web_port :: Integer} deriving (Show,Generic,A.FromJSON,A.ToJSON)

data Deadlines = Deadlines {deposit :: String, selectWinner :: String, payout :: String} deriving (Show,Generic,A.FromJSON,A.ToJSON)

main :: IO ()
main = do
  args <- getArgs  
  raffleConfiguration <- fromJust . A.decode @RaffleConfiguration <$> LBS.readFile (head args) 
  parties <- fromJust . A.decode @[AddressBech32] <$> LBS.readFile (args !! 1) 
  prizes <- fromJust . A.decode @[(PolicyId,TokenName)] <$> LBS.readFile (args !! 2)   
  s_address' <- C.unpack <$> (cat (sponsorAddressFilePath raffleConfiguration) |> captureTrim)
  let sponsor = Sponsor{ s_address = s_address'}
      oracle = Oracle{ o_address = s_address'}
  contractId <- genAndInitializeRaffle
    raffleConfiguration
    sponsor
    oracle
    parties
    prizes
  echo contractId
  
submit :: Sponsor -> RaffleConfiguration ->  IO ()
submit _sponsor raffleConfiguration = do
  cardano_cli 
    "transaction" 
    "sign"
    "--signing-key-file" (sponsorPrivateKeyFilePath raffleConfiguration)
    "--tx-body-file" (tmpTxToSign raffleConfiguration)
    "--out-file" (tmpTxToSubmit raffleConfiguration)
  echo " >> tx signed" &> StdErr
  marlowe_runtime_cli
    "--marlowe-runtime-host" (host . runtimeURI $ raffleConfiguration)
    "--marlowe-runtime-port" (proxy_port .  runtimeURI $ raffleConfiguration)
    "submit" 
    (tmpTxToSubmit raffleConfiguration)
  echo " >> tx submitted" &> StdErr

genAndInitializeRaffle :: RaffleConfiguration -> Sponsor -> Oracle -> [String] -> [(PolicyId,TokenName)] -> IO ContractId
genAndInitializeRaffle raffleConfiguration sponsor oracle parties prizes = do
  echo "#########################" &> StdErr
  echo "Raffle Contract Generation & Initialisation" &> StdErr
  echo "-------------------------" &> StdErr
  generateContract
  echo "-------------------------" &> StdErr
  echo "Raffle Contract Generated" &> StdErr
  echo "-------------------------" &> StdErr
  contractHash <- loadContractToStore
  contractId <- initialize contractHash
  echo "-------------------------" &> StdErr
  echo ("Raffle Contract Initialized : " ++ contractId ) &> StdErr
  echo "-------------------------" &> StdErr
  echo "#########################" &> StdErr
  return contractId
  where
    generateContract :: IO ()
    generateContract = do
      marlowe_cli
        "template"
        "raffle"
        "--minimum-ada" 2_000_000
        "--sponsor" (s_address sponsor)
        "--oracle"  (o_address oracle)
        "--chunk-size" (chunkSize raffleConfiguration)
        (asArg $ (\party -> ["--parties", party]) <$> parties)
        "--deposit-deadline" (deposit . deadlines $ raffleConfiguration)
        "--select-deadline"  (selectWinner . deadlines $ raffleConfiguration)
        "--payout-deadline"  (payout . deadlines $ raffleConfiguration)
        (asArg $ (\(a,b) -> ["--prizes", a ++ "." ++ b]) <$> prizes)
        "--out-contract-file" (contract raffleConfiguration)
        "--out-state-file" (state raffleConfiguration) 
      echo (" >> Raffle Contract saved in : " ++ contract raffleConfiguration) &> StdErr

    loadContractToStore = do
      contractHash <-
        C.unpack
          <$> ( marlowe_runtime_cli
                  "--marlowe-runtime-host" (host (runtimeURI raffleConfiguration))
                  "--marlowe-runtime-port" (proxy_port (runtimeURI raffleConfiguration))
                  "load"
                  (contract raffleConfiguration)
                  |> captureTrim
              )
      echo (" >> Contract stored with hash :" ++ contractHash) &> StdErr
      return contractHash

    initialize :: String -> IO ContractId
    initialize contractHash = do
      contractId <- C.unpack  <$> (marlowe_runtime_cli
        "--marlowe-runtime-host" (host (runtimeURI raffleConfiguration))
        "--marlowe-runtime-port" (proxy_port (runtimeURI raffleConfiguration))
        "create"
        "--min-utxo" 2_000_000
        "--change-address"  (s_address sponsor)
        "--manual-sign"     (tmpTxToSign raffleConfiguration)
        "--contract-hash"   contractHash |> captureTrim)
      echo contractId &> StdErr
      submit sponsor raffleConfiguration
      echo " >> Contract initialzed (tx appended)" &> StdErr
      return contractId
