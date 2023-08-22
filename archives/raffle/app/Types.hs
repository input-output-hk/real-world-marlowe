
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import GHC.Generics
import qualified Data.Aeson as A

type PolicyId = String

type TokenName = String

type ContractId = String

type RandomNumber = String

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

data RuntimeURI = RuntimeURI {proxy_host :: String, proxy_port :: Integer, web_host :: String, web_port :: Integer} deriving (Show,Generic,A.FromJSON,A.ToJSON)

data Deadlines = Deadlines {deposit :: String, selectWinner :: String, payout :: String} deriving (Show,Generic,A.FromJSON,A.ToJSON)

data PartyInfo
    = PartyInfo
    { asset_name :: String
    , payment_address :: AddressBech32
    , quantity :: String
    } deriving (Show,Generic,A.FromJSON,A.ToJSON)
