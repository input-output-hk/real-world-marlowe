
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Shh
import Text.Read (readMaybe)
import Tools
import Types
import qualified Data.ByteString.Lazy.Char8 as LBS8

submit :: Sponsor -> RaffleConfiguration ->  IO ()
submit _sponsor raffleConfiguration = do
  cardano_cli 
    "transaction" 
    "sign"
    "--signing-key-file" (sponsorPrivateKeyFilePath raffleConfiguration)
    "--tx-body-file" (tmpTxToSign raffleConfiguration)
    "--out-file" (tmpTxToSubmit raffleConfiguration)
  echo " >> tx signed" &> StdErr
  printResult
    =<< (
      marlowe_runtime_cli
        "--marlowe-runtime-host" (proxy_host . runtimeURI $ raffleConfiguration)
        "--marlowe-runtime-port" (proxy_port .  runtimeURI $ raffleConfiguration)
        "submit" 
        (tmpTxToSubmit raffleConfiguration)
        |> captureTrim
    )
  echo " >> tx submitted" &> StdErr

printResult :: LBS8.ByteString -> IO ()
printResult =
  maybe (putStrLn "Failed to parse result.") putStrLn
    . readMaybe . LBS8.unpack

