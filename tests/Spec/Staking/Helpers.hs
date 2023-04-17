{-# LANGUAGE NamedFieldPuns #-}

module Spec.Staking.Helpers
  ( findStakingManager,
    findStakingPosition,
    findAllStakingPositions,
  )
where

import GHC.Stack (HasCallStack)
import Indigo.Contracts.Staking.Common (StakingDatum (StakingPosition, owner))
import Indigo.Contracts.Staking.Common qualified as StakingParams
import Indigo.Utils.Helpers (hasUnitValue)
import Ledger qualified
import Plutus.Model
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude
import Spec.Staking.Script (StakingScript, stakingScript)
import Utils.Helpers (findUniqueUtxo, findUtxos)
import Utils.Mock qualified as Mock

findStakingManager :: HasCallStack => Run (V2.TxOutRef, V2.TxOut, StakingDatum)
findStakingManager = findUniqueUtxo (stakingScript Mock.stakeParams) condition
  where
    condition :: TxBox StakingScript -> Bool
    condition box =
      hasUnitValue
        (txBoxValue box)
        (StakingParams.stakingManagerNFT Mock.stakeParams)

findStakingPosition ::
  HasCallStack =>
  Ledger.PaymentPubKeyHash ->
  Run (V2.TxOutRef, V2.TxOut, StakingDatum)
findStakingPosition pk =
  findUniqueUtxo (stakingScript Mock.stakeParams) condition
  where
    condition :: TxBox StakingScript -> Bool
    condition box@TxBox {txBoxDatum = StakingPosition {owner}} =
      owner == pk
        && hasUnitValue
          (txBoxValue box)
          (StakingParams.stakingToken Mock.stakeParams)
    condition _ = False

findAllStakingPositions ::
  HasCallStack =>
  Ledger.PaymentPubKeyHash ->
  Run [(V2.TxOutRef, V2.TxOut, StakingDatum)]
findAllStakingPositions pk =
  findUtxos
    (stakingScript Mock.stakeParams)
    condition
  where
    condition :: TxBox StakingScript -> Bool
    condition box@TxBox {txBoxDatum = StakingPosition {owner}} =
      owner == pk
        && hasUnitValue
          (txBoxValue box)
          (StakingParams.stakingToken Mock.stakeParams)
    condition _ = False
