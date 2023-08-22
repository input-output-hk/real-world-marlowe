
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Common
import Control.Applicative ((<|>))
import Data.List.Utils (replace)
import Data.Maybe
import PyF
import Shh
import System.Environment (getArgs)
import Tools
import Types
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
  args <- getArgs  
  raffleConfiguration <- fromJust . A.decode @RaffleConfiguration <$> LBS.readFile (head args) 
  partyInfo <- fromJust . A.decode @[PartyInfo] <$> LBS.readFile (args !! 1) 
  prizes <- fromJust . A.decode @[(PolicyId,TokenName)] <$> LBS.readFile (args !! 2)
  let contractId = args !! 3   
  s_address' <- C.unpack <$> (cat (sponsorAddressFilePath raffleConfiguration) |> captureTrim)
  let sponsor = Sponsor{ s_address = s_address'}
      oracle = Oracle{ o_address = s_address'}
  runRaffleStateMachine
    raffleConfiguration
    sponsor
    oracle
    partyInfo
    prizes 
    contractId 

data ContractState 
  = WaitingForPrizeDeposit 
  | WaitingForOracle
  | WaitingForNotify
  | Close
  | Unknown String
  deriving (Show,Eq)

instance A.FromJSON ContractState where
  parseJSON =
    A.withObject "ContractState" $ \o ->
      let
        isClose =
          do
            e <- o A..: "errorCode"
            if e == "contractClosed"
              then pure Close
              else fail "Not closed"
        isWaiting =
          do
            a <- o A..: "applicable_inputs"
            let
              assertNotNull =
                A.withArray "Array" $ \v ->
                  if null v
                    then fail "Null array"
                    else pure ()
              isWaitingForOracle = 
                do
                  x <- a A..: "choices" :: A.Parser A.Value
                  assertNotNull x
                  pure WaitingForOracle
              isWaitingForPrizeDeposit =
                do
                  x <- a A..: "deposits"
                  assertNotNull x
                  pure WaitingForPrizeDeposit
              isWaitingForNotify =
                do
                  _ <- a A..: "notify" :: A.Parser A.Value
                  pure WaitingForNotify
            isWaitingForOracle <|> isWaitingForPrizeDeposit <|> isWaitingForNotify
        isUnknown = pure . Unknown . C.unpack $ A.encode o
      in
        isClose <|> isWaiting <|> isUnknown

getState :: RuntimeURI -> ContractId -> IO ContractState
getState runtime contractId = do
  validityStart <- C.unpack <$> (date "-u" "+\"%Y-%m-%dT%H:%M:%SZ\"" |> captureTrim)
  validityEnd <-   C.unpack <$> (date "-u" "-d" "+10 minutes" "+\"%Y-%m-%dT%H:%M:%SZ\"" |> captureTrim)
  let contractIdEncoded = replace "#" "%23" contractId
  nextQuery <- curl "-s"  "-H" "GET" 
                 [fmt|http://{(web_host $ runtime)}:{(show . web_port $ runtime)}/contracts/{contractIdEncoded}/next?validityStart={replace "\"" "" validityStart}&validityEnd={replace "\"" "" validityEnd}|]
                 |> captureTrim
  pure $ case A.decodeStrict $ C.toStrict nextQuery of
    Nothing -> Unknown $ C.unpack nextQuery
    Just state' -> state'

runRaffleStateMachine :: RaffleConfiguration -> Sponsor -> Oracle -> [PartyInfo] -> [(PolicyId,TokenName)]  -> ContractId -> IO()
runRaffleStateMachine = runRaffleStateMachine' False

runRaffleStateMachine' :: Bool -> RaffleConfiguration -> Sponsor -> Oracle  -> [PartyInfo] -> [(PolicyId,TokenName)]  -> ContractId ->  IO()
runRaffleStateMachine' True _ _ _ _ _ _ = return ()
runRaffleStateMachine' False raffleConfiguration sponsor oracle partyInfo prizes contractId = do
  let parties = payment_address <$> partyInfo
  sleep "5s" 
  state' <- getState (runtimeURI raffleConfiguration) contractId
  case state' of 
    WaitingForPrizeDeposit -> do
      echo ""
      echo "#########################"
      echo "WaitingForPrizeDeposit"
      sequence_ $ uncurry (depositNFT contractId) <$> prizes
      echo ">>>> Prize NFTs Deposited"
      echo "#########################"
      runRaffleStateMachine' False raffleConfiguration sponsor oracle partyInfo prizes contractId
      where 
            depositNFT :: ContractId -> PolicyId -> TokenName -> IO()
            depositNFT contractId' policyId tokenName = do
              echo $ " >> Depositing " ++ tokenName
              printResult =<< (
                marlowe_runtime_cli
                  "--marlowe-runtime-host" (proxy_host (runtimeURI raffleConfiguration))
                  "--marlowe-runtime-port" (proxy_port (runtimeURI raffleConfiguration))
                  "deposit"
                  "--change-address"  (s_address sponsor)
                  "--manual-sign"     (tmpTxToSign raffleConfiguration)
                  "--contract" contractId'
                  "--to-party" (s_address sponsor)
                  "--from-party" (s_address sponsor) 
                  "--currency" policyId
                  "--token-name" tokenName
                  "--quantity" 1
                  |> captureTrim)
              submit sponsor raffleConfiguration
              echo $ " >> " ++ tokenName ++ " deposited" 
    WaitingForOracle -> do
      echo ""
      echo "#########################"
      echo "WaitingForOracle"
      echo "#########################" 
      choiceMade <- provideRandomChoiceNumber
      echo $ ">>>> Oracle has answered with " ++ choiceMade
      let winner = partyInfo !! read choiceMade
          winningTicket = asset_name winner
          winningTicket' = B16.decode $ BS8.pack winningTicket
      echo $ ">>>> Winning ticket is " ++ winningTicket ++ either (const "") ((" (" ++) . (++ ")") . BS8.unpack) winningTicket'
      echo $ ">>>> Snapshotted address is " ++ payment_address winner
      echo "#########################" 
      runRaffleStateMachine' False raffleConfiguration sponsor oracle partyInfo prizes contractId 
       where 
        provideRandomChoiceNumber :: IO RandomNumber
        provideRandomChoiceNumber = do
          choiceMade <- C.unpack <$>
            ( curl
                "-s"  
                "-X" 
                "GET"
                [fmt|https://www.random.org/integers/?format=plain&col=1&rnd=new&base=10&num=1&min={0}&max={show $ (length parties)-1}|]
              |> captureTrim)
            
          printResult =<< (
            marlowe_runtime_cli
              "--marlowe-runtime-host" (proxy_host (runtimeURI raffleConfiguration))
              "--marlowe-runtime-port" (proxy_port (runtimeURI raffleConfiguration))
              "choose"
              "--change-address"  (s_address sponsor)
              "--manual-sign"     (tmpTxToSign raffleConfiguration)
              "--contract" contractId
              "--choice" "RANDOM"
              "--party" (s_address sponsor)
              "--value" choiceMade 
              |> captureTrim)
          submit sponsor raffleConfiguration
          return choiceMade
    WaitingForNotify -> do
      echo ""
      echo "#########################"
      echo "WaitingForNotify"
      echo "#########################"
      notify
      echo " >> Notified"
      echo "#########################"   
      runRaffleStateMachine' False raffleConfiguration sponsor oracle partyInfo prizes contractId
        where 
          notify  = do 
            printResult =<< (
              marlowe_runtime_cli
                "--marlowe-runtime-host" (proxy_host (runtimeURI raffleConfiguration))
                "--marlowe-runtime-port" (proxy_port (runtimeURI raffleConfiguration))
                "notify"
                "--change-address"  (s_address sponsor)
                "--manual-sign"     (tmpTxToSign raffleConfiguration)
                "--contract" contractId
                |> captureTrim)
            submit sponsor raffleConfiguration
             
    Close -> do 
      echo ""
      echo "#########################"
      echo "Raffle Closed"
      echo "#########################"  
      return () 
    Unknown payload -> do
      echo ""
      echo "#########################"
      echo "Unknown State : " 
      echo payload
      echo "#########################"  
      runRaffleStateMachine' False raffleConfiguration sponsor oracle partyInfo prizes contractId 
