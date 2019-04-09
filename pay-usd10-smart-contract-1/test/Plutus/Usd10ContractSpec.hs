{-# LANGUAGE OverloadedStrings #-}
module Plutus.Usd10ContractSpec (spec) where

import           Ledger
import           Ledger.Ada
import           Wallet.Emulator
import           Plutus.Usd10Contract
import           Control.Lens
import           Data.Either
import           Wallet.API
import           Test.Hspec
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "Usd10Contract" $ do
    let
        -- wallets
        buyerWallet = Wallet 194
        sellerWallet = Wallet 437
        exchangeWallet = Wallet 5575
        otherWallet = Wallet 235

        knownWallets = [buyerWallet, sellerWallet, exchangeWallet, otherWallet]

        buyerPubKey = walletPubKey buyerWallet
        sellerPubKey = walletPubKey sellerWallet
        exchangePubKey = walletPubKey exchangeWallet

        -- initial transaction
        initialTxPool = [initialTx]
        initialTx = Tx
              { txInputs = Set.empty
              , txOutputs = [
                  TxOutOf
                  { txOutAddress = pubKeyAddress buyerPubKey
                  , txOutValue = adaValueOf 500
                  , txOutType = PayToPubKey buyerPubKey
                  }
                ]
              , txForge = adaValueOf 500
              , txFee = fromInt 0
              , txValidRange = defaultSlotRange
              , txSignatures = Map.empty
              }

        -- Contract details
        adaToPay = 359
        range = Interval (Just (Slot 4)) (Just (Slot 9))
        oracle = OracleValue {
          ovSignature=exchangePubKey,
          ovSlot=Slot 4,
          ovValue=Usd10Payment {
            u10AdaToPay=adaValueOf adaToPay,
            u10ValidRange=range
          }
        }

        -- Helpers
        addr = contractAddress exchangePubKey sellerPubKey
        accept = acceptPayments exchangePubKey sellerPubKey
        buyWithOracle = buyUSD10 exchangePubKey sellerPubKey
        addBlock = do
                    txn <- processPending
                    _ <- walletsNotifyBlock knownWallets txn
                    pure ()
    it "accepts the right transaction" $ do
      let 
          (result, state) = runTraceTxPool initialTxPool $ do
              addBlock
              _ <- walletAction sellerWallet $ startWatching addr
              addBlock
              addBlock
              addBlock
              addBlock
              _ <- walletAction buyerWallet $ buyWithOracle oracle
              addBlock
              _ <- walletAction sellerWallet $ accept
              addBlock

      result `shouldSatisfy` isRight
      adaInWallet state buyerWallet `shouldBe` (500 - adaToPay)
      adaInWallet state sellerWallet `shouldBe` adaToPay

    it "rejects transactions that are too late" $ do
      let 
          (result, state) = runTraceTxPool initialTxPool $ do
              addBlock
              _ <- walletAction sellerWallet $ startWatching addr
              addBlock
              addBlock
              addBlock
              addBlock
              _ <- walletAction buyerWallet $ buyWithOracle oracle
              addBlock
              addBlock
              addBlock
              _ <- walletAction sellerWallet $ accept
              addBlock

      result `shouldSatisfy` isRight
      adaInWallet state buyerWallet `shouldBe` (500 - adaToPay)
      adaInWallet state sellerWallet `shouldBe` 0

    it "only seller can accept the payment" $ do
      let 
          (result, state) = runTraceTxPool initialTxPool $ do
              addBlock
              _ <- walletAction otherWallet $ startWatching addr
              addBlock
              addBlock
              addBlock
              addBlock
              _ <- walletAction buyerWallet $ buyWithOracle oracle
              addBlock
              _ <- walletAction otherWallet $ accept
              addBlock

      result `shouldSatisfy` isRight
      getErrors state `shouldBe` [ScriptFailure["Different destination"]]
      adaInWallet state buyerWallet `shouldBe` (500 - adaToPay)
      adaInWallet state sellerWallet `shouldBe` 0
      adaInWallet state otherWallet `shouldBe` 0

    it "rejects invalid oracles" $ do
      let 
          taintedOracle = oracle { ovSignature=buyerPubKey }
          (result, state) = runTraceTxPool initialTxPool $ do
              addBlock
              _ <- walletAction sellerWallet $ startWatching addr
              addBlock
              addBlock
              addBlock
              addBlock
              _ <- walletAction buyerWallet $ buyWithOracle taintedOracle
              addBlock
              _ <- walletAction sellerWallet $ accept
              addBlock

      result `shouldSatisfy` isRight
      getErrors state `shouldBe` [ScriptFailure["Invalid oracle"]]
      adaInWallet state buyerWallet `shouldBe` (500 - adaToPay)
      adaInWallet state sellerWallet `shouldBe` 0

    it "rejects transactions that pay an amount different to what the oracle states" $ do
      let 
          (result, state) = runTraceTxPool initialTxPool $ do
              addBlock
              _ <- walletAction sellerWallet $ startWatching addr
              addBlock
              addBlock
              addBlock
              addBlock
              _ <- walletAction buyerWallet $ payToScript_ defaultSlotRange addr (adaValueOf 5) (DataScript $ Ledger.lifted oracle)
              addBlock
              _ <- walletAction sellerWallet $ accept
              addBlock

      result `shouldSatisfy` isRight
      getErrors state `shouldBe` [ScriptFailure["Different amount of ADA"]]
      adaInWallet state buyerWallet `shouldBe` (500 - 5)
      adaInWallet state sellerWallet `shouldBe` 0

adaInWallet :: EmulatorState -> Wallet -> Int
adaInWallet es w = funds ws
  where
    ws = Map.lookup w $ _walletStates es
    funds Nothing = 0
    funds (Just ws') = sum $ (toInt . fromValue . txOutValue) <$> view ownFunds ws'

getErrors :: EmulatorState -> [ValidationError]
getErrors s = catMaybes $ toError <$> emLog s
  where
    toError (TxnValidationFail _ a) = Just a
    toError _ = Nothing
