{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Plutus.Usd10Contract where
import           Prelude                      hiding ((&&))
import qualified Language.PlutusTx            as PlutusTx
import qualified Language.PlutusTx.Prelude    as P
import           Ledger                       hiding (validRange)
import qualified Ledger.Value.TH              as Value
import qualified Ledger.Slot                  as Slot
import           Wallet

import           GHC.Generics
import qualified Data.Aeson as Aeson

data Usd10Payment = Usd10Payment {
    u10AdaToPay :: Value,
    u10ValidRange :: SlotRange
  } deriving (Generic, Aeson.ToJSON, Aeson.FromJSON)

PlutusTx.makeLift ''Usd10Payment

data Action
  = AcceptPayment

PlutusTx.makeLift ''Action

usd10Validator :: PubKey -> PubKey -> ValidatorScript
usd10Validator ex seller = ValidatorScript $ validator `applyScript` (Ledger.lifted ex) `applyScript` (Ledger.lifted seller)
  where
    validator = ($$(Ledger.compileScript [||
      \ (exchangePk :: PubKey)
        (sellerPk :: PubKey)
        (ds :: OracleValue Usd10Payment)
        (a :: Action)
        (tx :: PendingTx) ->
      let

          (&&) :: Bool -> Bool -> Bool
          (&&) = $$(P.and)

          failWith :: String -> a
          failWith s = $$(P.traceH) s ($$(P.error) ())

          isValidExchangePk :: PubKey -> Bool
          isValidExchangePk pk = $$(eqPubKey) pk exchangePk

          PendingTx _ pendingTxOuts _ _ _ pTxValidRange _ _= tx

          verifyOracle :: OracleValue a -> (Slot, a)
          verifyOracle (OracleValue pk h t) | isValidExchangePk pk = (h, t)
          verifyOracle _ = failWith "Invalid oracle"

          (_, Usd10Payment {
            u10AdaToPay=ada,
            u10ValidRange=validRange  
          }) = verifyOracle ds

      in
      case a of
        AcceptPayment ->
          let
              -- Check that the oracle is valid for the transaction
              slotIsRight = $$(Slot.contains) validRange pTxValidRange

              -- Check that is the seller the one claiming the transaction
              destinationIsSeller = case pendingTxOuts of
                [PendingTxOut {pendingTxOutData=PubKeyTxOut pk}] | $$(eqPubKey) pk sellerPk -> True
                [PendingTxOut {pendingTxOutData=PubKeyTxOut _}] -> failWith "Different destination"
                _ -> failWith "More than one destination"

              -- Check that the amount being paid is the same as the oracle states
              amountIsRight = case pendingTxOuts of
                [PendingTxOut {pendingTxOutValue=val}] | $$(Value.eq) val ada -> True
                [PendingTxOut {pendingTxOutData=_}] -> failWith "Different amount of ADA"
                _ -> failWith "More than one destination"
          in
          if slotIsRight && destinationIsSeller && amountIsRight
          then ()
          else $$(P.error) ()

      ||]))

contractAddress :: PubKey -> PubKey -> Address
contractAddress expk sepk = Ledger.scriptAddress (usd10Validator expk sepk)

buyUSD10 :: (WalletAPI m, WalletDiagnostics m) => PubKey -> PubKey -> OracleValue Usd10Payment -> m ()
buyUSD10 expk sepk o = payToScript_ defaultSlotRange (contractAddress expk sepk) vl ds
  where
    vl = u10AdaToPay $ ovValue o
    ds = DataScript (lifted o)

acceptPayments :: (WalletAPI m, WalletDiagnostics m) => PubKey -> PubKey -> m ()
acceptPayments expk sepk = do
                  s <- slot
                  collectFromScript (mkInterval s) (usd10Validator expk sepk) (RedeemerScript $ lifted AcceptPayment)
  where
    mkInterval :: Slot -> Interval Slot
    mkInterval (Slot s) = Interval (Just (Slot s)) (Just (Slot $ s + 3))
