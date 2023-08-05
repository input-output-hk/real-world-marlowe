
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Shh
import Tools
import Types

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

