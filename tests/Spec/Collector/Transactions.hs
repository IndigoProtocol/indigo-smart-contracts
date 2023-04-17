{-# OPTIONS_GHC -Wno-unused-imports #-}

module Spec.Collector.Transactions
  ( runInitCollector,
    runInitCollectorWithStakeCredential,
    collectorFeeTx,
    runSendFeeToCollector,
    runCreateNewCollectorUtxo,
    runInitRefScriptCollector,
    runDirectlySpendFunds,
    runCollectorDoubleSatisfaction,
    runDirectlySpendFundsExploit,
  )
where

import Control.Monad
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import GHC.Stack (HasCallStack)
import Indigo.Contracts.Collector.Common (CollectorRedeemer (Collect))
import Ledger (Value)
import Ledger.Ada qualified as Ada
import Plutus.Model
import Plutus.V2.Ledger.Api (PubKeyHash)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude hiding (Semigroup ((<>)), error, mconcat, pure)
import Spec.Collector.Script (collectorScript)
import Utils.Helpers (findAllUtxosAt, findUniqueUtxo, getFirstRefScript, initScriptRef, minLovelacesPerUtxo, unwrapAppendStaking)
import Utils.Mock qualified as Mock
import Prelude (Int, Semigroup ((<>)), drop, error, mconcat, pure, replicate)

runInitRefScriptCollector :: Run ()
runInitRefScriptCollector = initScriptRef (collectorScript Mock.collectorParams)

runInitCollector :: HasCallStack => Int -> Maybe Integer -> Run ()
runInitCollector collectorUtxosCount amount = do
  runInitRefScriptCollector
  admin <- getMainUser
  let val = Ada.lovelaceValueOf $ minLovelacesPerUtxo + fromMaybe 0 amount
  sp <- spend admin (mconcat $ replicate collectorUtxosCount val)
  let tx =
        mconcat
          [ userSpend sp,
            mconcat $
              replicate
                collectorUtxosCount
                ( payToRef
                    (collectorScript Mock.collectorParams)
                    (InlineDatum ())
                    val
                )
          ]
  void $ signTx admin tx >>= sendTx

runInitCollectorWithStakeCredential ::
  HasCallStack => PubKeyHash -> Int -> Maybe Integer -> Run ()
runInitCollectorWithStakeCredential user collectorUtxosCount amount = do
  runInitRefScriptCollector
  admin <- getMainUser
  let val = Ada.lovelaceValueOf $ minLovelacesPerUtxo + fromMaybe 0 amount
  sp <- spend admin (mconcat $ replicate collectorUtxosCount val)
  let tx =
        mconcat
          [ userSpend sp,
            mconcat $
              replicate
                collectorUtxosCount
                ( payToRef
                    (appendStakingPubKey user $ collectorScript Mock.collectorParams)
                    (InlineDatum ())
                    val
                )
          ]
  void $ signTx admin tx >>= sendTx

collectorFeeTx :: Int -> Value -> Run Tx
collectorFeeTx utxoIdx fee' = do
  refScriptUtxo <- getFirstRefScript (collectorScript Mock.collectorParams)
  utxos <- utxoAt (collectorScript Mock.collectorParams)
  (oref, o) <-
    maybe
      (error "No collector script UTXOs or wrong index")
      (\(res :| _) -> return res)
      (nonEmpty (drop utxoIdx utxos))
  pure $
    mconcat
      [ spendScriptRef
          refScriptUtxo
          (collectorScript Mock.collectorParams)
          oref
          Collect
          (),
        payToRef
          (collectorScript Mock.collectorParams)
          (InlineDatum ())
          (V2.txOutValue o <> fee')
      ]

-- | This is a helper function used only for testing.
runSendFeeToCollector :: Int -> Integer -> Run ()
runSendFeeToCollector utxoIdx lovelaces = do
  admin <- getMainUser
  let val = Ada.lovelaceValueOf lovelaces
  sp <- spend admin val
  tx <- collectorFeeTx utxoIdx val
  void $ signTx admin (userSpend sp <> tx) >>= sendTx

runCreateNewCollectorUtxo :: Integer -> Run ()
runCreateNewCollectorUtxo lovelaces = do
  admin <- getMainUser
  let spendVal =
        Ada.lovelaceValueOf (lovelaces + minLovelacesPerUtxo)
  sp <- spend admin spendVal
  let tx =
        mconcat
          [ userSpend sp,
            payToRef
              (collectorScript Mock.collectorParams)
              (InlineDatum ())
              spendVal
          ]
  void $ signTx admin tx >>= sendTx

-- Attempt to spend the Collector funds from the Collector UTxO directly into users wallet.
runDirectlySpendFunds :: Run ()
runDirectlySpendFunds = do
  let val = Ada.lovelaceValueOf 20_000_000
  runInitCollector 1 (Just 20_000_000)

  admin <- getMainUser
  refScriptUtxo <- getFirstRefScript (collectorScript Mock.collectorParams)
  rewardUtxos <- utxoAt (collectorScript Mock.collectorParams)

  let tx =
        mconcat
          [ mconcat
              [ spendScriptRef
                  refScriptUtxo
                  (collectorScript Mock.collectorParams)
                  coref
                  Collect
                  ()
                | (coref, _) <- rewardUtxos
              ],
            payToKey admin (val <> Ada.lovelaceValueOf minLovelacesPerUtxo)
          ]
  void $ signTx admin tx >>= sendTx

runDirectlySpendFundsExploit :: Run ()
runDirectlySpendFundsExploit = do
  let val = Ada.lovelaceValueOf 20_000_000 <> Ada.lovelaceValueOf minLovelacesPerUtxo
  admin <- getMainUser

  runInitCollector 1 (Just 20_000_000)
  runInitCollectorWithStakeCredential admin 2 (Just 20_000_000)

  refScriptUtxo <- getFirstRefScript (collectorScript Mock.collectorParams)

  rewardUtxos <- utxoAt (collectorScript Mock.collectorParams)
  rewardUtxos1 <- utxoAt (appendStakingPubKey admin $ collectorScript Mock.collectorParams)

  let tx =
        mconcat
          [ mconcat
              [ spendScriptRef
                  refScriptUtxo
                  (collectorScript Mock.collectorParams)
                  cref
                  Collect
                  ()
                | (cref, _) <- rewardUtxos <> rewardUtxos1
              ],
            payToKey admin val, -- user steals the ada
            payToRef
              (collectorScript Mock.collectorParams)
              (InlineDatum ())
              val,
            payToRef
              (appendStakingPubKey admin $ collectorScript Mock.collectorParams)
              (InlineDatum ())
              val
          ]
  void $ signTx admin tx >>= sendTx

-- Creates two Collector UTxOs, attempts to increase the value of one and
-- withdraw the rest of the funds to the users wallet.
runCollectorDoubleSatisfaction :: Run ()
runCollectorDoubleSatisfaction = do
  let valToReturn = Ada.lovelaceValueOf 30_000_000
      valToUser = Ada.lovelaceValueOf 10_000_000

  runInitCollector 2 (Just 20_000_000)

  admin <- getMainUser
  refScriptUtxo <- getFirstRefScript (collectorScript Mock.collectorParams)
  rewardUtxos <- utxoAt (collectorScript Mock.collectorParams)

  let tx =
        mconcat
          [ mconcat
              [ spendScriptRef
                  refScriptUtxo
                  (collectorScript Mock.collectorParams)
                  coref
                  Collect
                  ()
                | (coref, _) <- rewardUtxos
              ],
            payToKey admin (valToUser <> Ada.lovelaceValueOf (2 * minLovelacesPerUtxo)),
            payToRef (collectorScript Mock.collectorParams) (InlineDatum ()) valToReturn
          ]
  void $ signTx admin tx >>= sendTx
