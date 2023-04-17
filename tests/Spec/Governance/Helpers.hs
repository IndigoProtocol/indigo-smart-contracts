{-# LANGUAGE NamedFieldPuns #-}

module Spec.Governance.Helpers
  ( versionRecordOutputValue,
    findGov,
    findPollManagerWithId,
    findAllPollShardsWithId,
    findFirstPollShardWithId,
    findUpgradeWithId,
    findVersionRecordByValHash,
  )
where

import Data.Function (on)
import Data.List (maximumBy)
import Data.List.NonEmpty (nonEmpty)
import GHC.Stack (HasCallStack)
import Indigo.Contracts.Governance.Execute.Common qualified as Execute
import Indigo.Contracts.Governance.Gov.Common qualified as GovParams
import Indigo.Contracts.Governance.Poll.Common qualified as Poll
import Indigo.Contracts.Governance.VersionRegistry.Common
  ( VersionRecord (VersionRecord, versionId, versionPaths),
  )
import Indigo.Utils.Helpers (hasUnitValue, unitValue)
import Indigo.Utils.Spooky qualified as Spooky
import Ledger qualified
import Ledger.Ada qualified as Ada
import Plutus.Model
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (pure)
import Spec.Governance.Script
  ( ExecuteScript,
    GovScript,
    PollManagerScript,
    PollScript,
    VersionRegistryScript,
    executeScript,
    govScript,
    pollManagerScript,
    pollScript,
    versionRegistryScript,
  )
import Utils.Helpers (findUniqueUtxo, findUtxos, minLovelacesPerUtxo)
import Utils.Mock qualified as Mock
import Prelude (pure)

versionRecordOutputValue :: Ledger.Value
versionRecordOutputValue =
  unitValue versionRecordToken <> Ada.lovelaceValueOf minLovelacesPerUtxo
  where
    Execute.ExecuteParams {Execute.versionRecordToken} = Mock.executeParams

findGov :: Run (V2.TxOutRef, V2.TxOut, GovParams.GovDatum)
findGov = findUniqueUtxo (govScript Mock.govParams) condition
  where
    condition :: TxBox GovScript -> Bool
    condition box =
      hasUnitValue (txBoxValue box) (GovParams.govNFT Mock.govParams)

findPollManagerWithId ::
  Integer -> Run (V2.TxOutRef, V2.TxOut, Poll.PollManager)
findPollManagerWithId idx =
  findUniqueUtxo (pollManagerScript Mock.pollManagerParams) condition
  where
    condition :: TxBox PollManagerScript -> Bool
    condition box@TxBox {txBoxDatum = Poll.PollManager {Poll.pId}} =
      idx == pId
        && hasUnitValue (txBoxValue box) (Spooky.unSpookyAssetClass pollToken)
    Poll.PollParams {Poll.pollToken} = Mock.pollParams

findAllPollShardsWithId ::
  Integer -> Run [(V2.TxOutRef, V2.TxOut, Poll.PollShard)]
findAllPollShardsWithId idx =
  findUtxos (pollScript Mock.pollParams) condition
  where
    condition :: TxBox PollScript -> Bool
    condition box@TxBox {txBoxDatum = Poll.PollShard {Poll.psId}} =
      idx == psId
        && hasUnitValue (txBoxValue box) (Spooky.unSpookyAssetClass pollToken)
    Poll.PollParams {Poll.pollToken} = Mock.pollParams

findFirstPollShardWithId ::
  Integer -> Run (V2.TxOutRef, V2.TxOut, Poll.PollShard)
findFirstPollShardWithId idx = do
  candidates <- findAllPollShardsWithId idx
  -- TODO: when porting to CTL change `head`
  -- to a random index to get random poll shard
  pure $ head candidates

findUpgradeWithId ::
  HasCallStack => Integer -> Run (V2.TxOutRef, V2.TxOut, Execute.Upgrade)
findUpgradeWithId idx =
  findUniqueUtxo (executeScript Mock.executeParams) condition
  where
    condition :: TxBox ExecuteScript -> Bool
    condition box@TxBox {txBoxDatum = Execute.Upgrade {Execute.uId}} =
      uId == idx
        && hasUnitValue
          (txBoxValue box)
          (Execute.upgradeToken Mock.executeParams)

findVersionRecordByValHash ::
  Ledger.ValidatorHash ->
  Run (Maybe (V2.TxOutRef, V2.TxOut, VersionRecord))
findVersionRecordByValHash valHash = do
  records <- findUtxos versionRegistryScript condition
  pure (maximumBy (compare `on` foundId) <$> nonEmpty records)
  where
    condition :: TxBox VersionRegistryScript -> Bool
    condition box@TxBox {txBoxDatum = VersionRecord {versionPaths}} =
      valHash `AssocMap.member` versionPaths
        && hasUnitValue
          (txBoxValue box)
          (Execute.versionRecordToken Mock.executeParams)

    foundId :: (V2.TxOutRef, V2.TxOut, VersionRecord) -> Integer
    foundId (_, _, VersionRecord {versionId}) = versionId
