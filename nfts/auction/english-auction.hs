
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Trustworthy        #-}

module Main (
-- * Entry point
  main
-- * Contracts
, makeContract
) where

import Control.Monad (foldM)
import Control.Monad.Writer (Writer, runWriter)
import Data.Aeson ((.=), encodeFile, object, toJSON)
import Data.List.Split (chunksOf)
import Data.Map.Strict (toList)
import Data.String (fromString)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.CLI.Merkle (deepMerkleize)
import Language.Marlowe.CLI.Types (Continuations)
import Plutus.V1.Ledger.Api (POSIXTime(..), TokenName(..))

-- | Print the contract.
main :: IO ()
main =
  do
    let
      (contract, continuations) =
        runWriter
          $ makeContract
            6 (Role <$> ["f.beaumont", "e.cary", "m.herbert", "j.lumley", "j.webster"])
            (Bound 2_000_000 1_000_000_000_000)
            (Token "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d" "BearGarden")
            1678573569000 (5 * 3600000)
    encodeFile "contract.json" contract
    encodeFile "continuations.json" continuations

-- | A timeout beyond the operation of the contract, just used for merklization.
timeoutFinal :: POSIXTime
timeoutFinal = POSIXTime 1678595169000

-- | Ada.
ada :: Token
ada = Token "" ""

-- | The party that sells the item at auction.
seller :: Party
seller = Role "c.marlowe"

-- | The quantity of items that is auctioned.
assetAmount :: Value Observation
assetAmount = Constant 1

-- | The minimum Ada requirement for the asset.
minAda :: Value Observation
minAda = Constant 2000000

-- | The value of the highest bid.
highestBid :: ValueId
highestBid = "Highest Bid"

-- | Create the Marlowe contract for an English auction.
makeContract :: Int                            -- ^ The number of rounds of bidding.
             -> [Party]                        -- ^ The bidders.
             -> Bound                          -- ^ The range for valid bids, in Lovelace.
             -> Token                          -- ^ The token representing the asset being bid upon.
             -> Integer                        -- ^ The start time of the contract.
             -> Integer                        -- ^ The spacing between bid deadlines.
             -> Writer Continuations Contract  -- ^ Action for creating the merkleized English auction.
makeContract nRounds bidders bidBounds assetToken start delta =
  do
    let
      bids = ChoiceId "Bid" <$> bidders
      deadlines = [POSIXTime $ start + delta * (1 + fromIntegral i) | i <- [1..nRounds]]
    -- Make explicit payments upon completion of bidding.
    close <- foldM (flip makeRefunds) Close (chunksOf 2 $ seller : bidders)
    -- Deposit the asset, then make the bids, but close if no one bids.
    makeAssetDeposit assetToken (POSIXTime $ start + delta)
      -- Do the rounds of bidding and depositing.
      =<< makeBids bidBounds assetToken deadlines bids
      close close

-- | Deposit the asset that is the subject of the bidding.
makeAssetDeposit :: Token                          -- ^ The token representing the asset being bid upon.
                 -> Timeout                        -- ^ The timeout for depositing the asset.
                 -> Contract                       -- ^ The contract to be executed after the asset is deposited.
                 -> Writer Continuations Contract  -- ^ Action for creating the merkleized contract for the asset deposit and subsequent activity.
makeAssetDeposit asset assetDeadline continuation =
  deepMerkleize
    $ When
    [
      -- The seller deposits the asset being auctioned.
      Case (Deposit seller seller asset assetAmount)
        continuation
    ]
    -- The contract ends if the deposit is not made.
    assetDeadline
    Close

-- | Make the contract for bids.
makeBids :: Bound                            -- ^ The range of valid bids, in Lovelace.
         -> Token                            -- ^ The token representing the asset being bid upon.
         -> [Timeout]                        -- ^ The deadlines for the rounds of bidding.
         -> [ChoiceId]                       -- ^ The choices the bidders will make.
         -> Contract                         -- ^ The contract to be executed at the end of the bidding.
         -> Contract                         -- ^ The next stage of the contract, if a valid bid was not made.
         -> Writer Continuations Contract    -- ^ Action for creating the merkleized bidding contract, and its merkleized continuations.
makeBids _ _ [] _ _ continuation = pure continuation
makeBids _ _ _ [] _ continuation = pure continuation
makeBids bounds assetToken (deadline : remainingDeadlines) bids close continuation =
  do
    continuation' <- merkleizeTimeout continuation
    let
      -- Let a bidder make their bid.
      makeBid bid@(ChoiceId _ bidder) =
        do
          let
            bidAmount = ChoiceValue bid
          -- Handle the remaining bids and finalization if the bidder is not disqualified.
          remaining <-
            makeBids bounds assetToken remainingDeadlines bids close
              $ When
                [
                  -- Wait for a notification before continuing.
                  Case (Notify TrueObs)
                    -- Make the payment for the asset.
                    $ Pay bidder (Account seller) ada bidAmount
                    $ Pay seller (Party bidder) assetToken assetAmount
                    -- Close the contract.
                    close
                ]
                timeoutFinal Close
          -- Handle the remaining bids and finalization if the bidder is disqualified.
          disqualify <- makeBids bounds assetToken remainingDeadlines (filter (/= bid) bids) close continuation'
          disqualify' <- merkleizeTimeout disqualify
          -- Require a deposit if a bid was made.
          deposit <-
            deepMerkleize
              $ When
                [
                  -- Deposit the Lovelace for the bid.
                  Case (Deposit bidder bidder ada . SubValue (AddValue bidAmount minAda) $ AvailableMoney bidder ada)
                    -- Record the new highest amount.
                    $ Let highestBid bidAmount
                    -- Handle the remaining bids.
                      remaining
                ]
                -- Ignore the bid and disqualify the bidder if the deposit was not made.
                deadline
                disqualify'
          pure
            $ Case (Choice bid [bounds])
              -- Check if the bid is highest so far.
              $ If (ValueGT bidAmount $ UseValue highestBid)
                  -- Require a deposit if the bid is highest.
                  deposit
                  -- Ignore the bid and disqualify the bidder if it is not highest.
                  disqualify
    cs <- mapM makeBid bids
    deepMerkleize
      $ When cs 
      -- End the bidding if no one bids in this round.
      deadline
      continuation'

-- | Make an explicit refund.
makeRefunds :: [Party]                        -- ^ The parties to refund.
            -> Contract                       -- ^ The contract to be executed after the refund.
            -> Writer Continuations Contract  -- ^ Action to merklieze the continuation.
makeRefunds parties continuation =
  deepMerkleize
    $ When
      [
        -- Wait for a notification before continuing.
        Case (Notify TrueObs)
          -- Pay everyone.
          $ flip (foldr (\party -> Pay party (Party party) ada (AvailableMoney party ada))) parties
            continuation
      ]
      timeoutFinal Close

-- | Merkleize a timeout continuations.
merkleizeTimeout :: Contract                       -- ^ The continuation to be merkleized.
                 -> Writer Continuations Contract  -- ^ Action to merklieze the continuation.
merkleizeTimeout continuation =
  deepMerkleize
    $ When [Case (Notify TrueObs) continuation] timeoutFinal Close

