
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Common
import Data.Maybe
import Shh
import System.Environment (getArgs)
import Tools
import Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
  args <- getArgs  
  raffleConfiguration <- fromJust . A.decode @RaffleConfiguration <$> LBS.readFile (head args) 
  partyInfo <- fromJust . A.decode @[PartyInfo] <$> LBS.readFile (args !! 1) 
  let parties = payment_address <$> partyInfo
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
  printResult $ C.pack contractId
  
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
  echo ("Raffle Contract Initialized : " ++ read contractId ) &> StdErr
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
                  "--marlowe-runtime-host" (proxy_host (runtimeURI raffleConfiguration))
                  "--marlowe-runtime-port" (proxy_port (runtimeURI raffleConfiguration))
                  "load"
                  "--read-json"
                  (contract raffleConfiguration)
                  |> captureTrim
              )
      echo (" >> Contract stored with hash: " ++ contractHash) &> StdErr
      return contractHash

    initialize :: String -> IO ContractId
    initialize contractHash = do
      contractId <- C.unpack  <$> (marlowe_runtime_cli
        "--marlowe-runtime-host" (proxy_host (runtimeURI raffleConfiguration))
        "--marlowe-runtime-port" (proxy_port (runtimeURI raffleConfiguration))
        "create"
        "--min-utxo" 2_000_000
        "--change-address"  (s_address sponsor)
        "--manual-sign"     (tmpTxToSign raffleConfiguration)
        "--contract-hash"   contractHash |> captureTrim)
      echo (read contractId :: String) &> StdErr
      submit sponsor raffleConfiguration
      echo " >> Contract initialzed (tx appended)" &> StdErr
      return contractId
