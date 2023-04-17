{-# LANGUAGE NamedFieldPuns #-}

module Spec.StabilityPool.Helpers
  ( findAccount,
    findStabilityPool,
    findSnapshotsEpochToScaleToSum,
    adjustmentHelper,
  )
where

import GHC.Stack (HasCallStack)
import Indigo.Contracts.StabilityPool.Common
  ( EpochToScaleToSum,
    StabilityDatum
      ( AccountDatum,
        SnapshotEpochToScaleToSumDatum,
        StabilityPoolDatum,
        accIAsset,
        accOwner,
        accSnapshot,
        epochToScaleToSum,
        sessAsset,
        sessSnapshot,
        spIAsset,
        spSnapshot
      ),
    StabilityPoolParams
      ( accountToken,
        snapshotEpochToScaleToSumToken,
        stabilityPoolToken
      ),
    StabilityPoolSnapshot (StabilityPoolSnapshot, snapshotEpoch, snapshotScale),
    adjustAccountHelper,
  )
import Indigo.Utils.Helpers (hasUnitValue)
import Ledger qualified
import Plutus.Model
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (pure, scale)
import Spec.StabilityPool.Script (StabilityPoolScript, stabilityPoolScript)
import Utils.Helpers (findUniqueUtxo, findUniqueUtxo')
import Utils.Mock qualified as Mock
import Prelude (pure)

findAccount ::
  HasCallStack =>
  Ledger.PaymentPubKeyHash ->
  Ledger.TokenName ->
  Run (V2.TxOutRef, V2.TxOut, StabilityPoolSnapshot)
findAccount pk asset = do
  (oref, o, AccountDatum {accSnapshot}) <-
    findUniqueUtxo (stabilityPoolScript Mock.spParams) condition
  return (oref, o, accSnapshot)
  where
    condition :: TxBox StabilityPoolScript -> Bool
    condition
      box@TxBox
        { txBoxDatum = AccountDatum {accOwner, accIAsset}
        } =
        asset == accIAsset
          && pk == accOwner
          && hasUnitValue (txBoxValue box) (accountToken Mock.spParams)
    condition _ = False

findStabilityPool ::
  HasCallStack =>
  Ledger.TokenName ->
  Run (V2.TxOutRef, V2.TxOut, StabilityPoolSnapshot, EpochToScaleToSum)
findStabilityPool asset = do
  (oref, o, datum) <-
    findUniqueUtxo (stabilityPoolScript Mock.spParams) condition
  let StabilityPoolDatum {spSnapshot, epochToScaleToSum} = datum
  return (oref, o, spSnapshot, epochToScaleToSum)
  where
    condition :: TxBox StabilityPoolScript -> Bool
    condition box@TxBox {txBoxDatum = StabilityPoolDatum {spIAsset}} =
      spIAsset == asset
        && hasUnitValue (txBoxValue box) (stabilityPoolToken Mock.spParams)
    condition _ = False

type SnapshotESSSearchResult = (V2.TxOutRef, V2.TxOut, EpochToScaleToSum)

findSnapshotsEpochToScaleToSum ::
  HasCallStack =>
  Ledger.TokenName ->
  StabilityPoolSnapshot ->
  Run (SnapshotESSSearchResult, Maybe SnapshotESSSearchResult)
findSnapshotsEpochToScaleToSum
  asset
  StabilityPoolSnapshot {snapshotEpoch, snapshotScale} = do
    (oref1, o1, SnapshotEpochToScaleToSumDatum {sessSnapshot = snap1}) <-
      findUniqueUtxo
        (stabilityPoolScript Mock.spParams)
        (condition snapshotEpoch snapshotScale)
    res <-
      findUniqueUtxo'
        (stabilityPoolScript Mock.spParams)
        (condition snapshotEpoch (snapshotScale + 1))
    return
      ( (oref1, o1, snap1),
        res
          >>= ( \( oref2,
                   o2,
                   SnapshotEpochToScaleToSumDatum
                     { sessSnapshot = snap2
                     }
                   ) ->
                    if oref1 == oref2
                      then Nothing
                      else Just (oref2, o2, snap2)
              )
      )
    where
      condition :: Integer -> Integer -> TxBox StabilityPoolScript -> Bool
      condition
        epoch
        scale
        box@TxBox
          { txBoxDatum =
              SnapshotEpochToScaleToSumDatum {sessSnapshot, sessAsset}
          } =
          sessAsset == asset
            && AssocMap.member (epoch, scale) sessSnapshot
            && hasUnitValue
              (txBoxValue box)
              (snapshotEpochToScaleToSumToken Mock.spParams)
      condition _ _ _ = False

-- | Helper for account adjustment.
adjustmentHelper ::
  HasCallStack =>
  Ledger.TokenName ->
  StabilityPoolSnapshot ->
  EpochToScaleToSum ->
  StabilityPoolSnapshot ->
  Run (StabilityPoolSnapshot, Integer, [V2.TxOutRef])
adjustmentHelper asset snapshot epochToScaleToSum accountSnapshot = do
  (accumulatedEpochToScaleToSum, refInputs) <-
    if AssocMap.member
      (snapshotEpoch accountSnapshot, snapshotScale accountSnapshot)
      epochToScaleToSum
      then pure (AssocMap.toList epochToScaleToSum, [])
      else do
        ((snapshotEssOref1, _, snap1), essSearchRes2) <-
          findSnapshotsEpochToScaleToSum asset accountSnapshot
        case essSearchRes2 of
          Nothing ->
            pure
              ( foldMap AssocMap.toList [snap1, epochToScaleToSum],
                [snapshotEssOref1]
              )
          Just (snapshotEssOref2, _, snap2) ->
            pure
              ( foldMap AssocMap.toList [snap2, snap1, epochToScaleToSum],
                [ snapshotEssOref1,
                  snapshotEssOref2
                ]
              )
  let (newAccountSnapshot, rew) =
        adjust (AssocMap.fromList accumulatedEpochToScaleToSum)
  pure (newAccountSnapshot, rew, refInputs)
  where
    adjust :: EpochToScaleToSum -> (StabilityPoolSnapshot, Integer)
    adjust ess =
      let (newAccountSnapshot, rew) =
            adjustAccountHelper snapshot accountSnapshot ess
       in (newAccountSnapshot, fromMaybe 0 rew)
