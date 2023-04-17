-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-
Common functions, data structures for Staking Script.
-}

module Indigo.Contracts.Staking.Common
  ( stakingManagerTokenName,
    stakingTokenName,
    StakingParams
      ( StakingParams,
        stakingManagerNFT,
        stakingToken,
        indyToken,
        pollToken,
        versionRecordToken,
        collectorValHash
      ),
    RewardSnapshot (RewardSnapshot, snapshotAda),
    StakingDatum
      ( StakingManager,
        StakingPosition,
        totalStake,
        mSnapshot,
        owner,
        lockedAmount,
        pSnapshot
      ),
    StakingRedeemer
      ( CreateStakingPosition,
        UpdateTotalStake,
        Distribute,
        AdjustStakedAmount,
        Unstake,
        Lock,
        Unlock,
        UpgradeVersion
      ),
    Staking,
    removeExpiredLockedAmount,
    getReward,
    distributeReward,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Indigo.Data.Decimal
  ( DivideSemigroup (divide),
    OnChainDecimal (OnChainDecimal, getOnChainInt),
  )
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (ValidatorHash)
import PlutusTx qualified
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (divide, toList)
import Prelude qualified as P

stakingManagerTokenName :: Value.TokenName
stakingManagerTokenName = Value.TokenName "staking_manager_nft"

stakingTokenName :: Value.TokenName
stakingTokenName = Value.TokenName "staking_token"

data StakingParams = StakingParams
  { -- | NFT identifying authentic Staking Manager output
    stakingManagerNFT :: Value.AssetClass,
    -- | Token identifying authentic staking position output
    stakingToken :: Value.AssetClass,
    -- | AssetClass of INDY token
    indyToken :: Value.AssetClass,
    -- | Token identifying authentic Poll output
    pollToken :: Value.AssetClass,
    -- | Token identifying the VersionRegistry output
    versionRecordToken :: Value.AssetClass,
    -- | Collector Script
    collectorValHash :: ValidatorHash
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord, OpenApi.ToSchema)

PlutusTx.makeLift ''StakingParams
PlutusTx.makeIsDataIndexed ''StakingParams [('StakingParams, 0)]

data RewardSnapshot = RewardSnapshot
  { snapshotAda :: OnChainDecimal
  }
  deriving (Generic, FromJSON, ToJSON, P.Show)

PlutusTx.makeLift ''RewardSnapshot
PlutusTx.makeIsDataIndexed ''RewardSnapshot [('RewardSnapshot, 0)]

{-
There are two kinds of output being locked at this script
1. StakingManager: Store some aggregate information of the Staking Contract.
To create a new staking positon, users must consume the StakingManager output
in the transaction.
2. StakingPosition: Each output of this type represents staking account/profile
of a PubKeyHash. All INDY tokens will be stored at StakingPosition outputs.
-}
data StakingDatum
  = StakingManager
      { -- total INDY locked at the Staking Contract.
        totalStake :: Integer,
        mSnapshot :: RewardSnapshot
      }
  | StakingPosition
      { owner :: Ledger.PaymentPubKeyHash,
        -- The INDY tokens being locked for voting in the Governance Contract.
        -- It's a map:
        -- Proposal/Poll Id -> (Vote Amount, Proposal/Poll end of voting period)
        lockedAmount :: Map Integer (Integer, Ledger.POSIXTime),
        pSnapshot :: RewardSnapshot
      }
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''StakingDatum
PlutusTx.makeIsDataIndexed
  ''StakingDatum
  [('StakingManager, 0), ('StakingPosition, 1)]

{-
Staking Redeemer Action:
1. CreateStakingPosition:
Create an individual staking profile for a new PubKeyHash.
2. UpdateTotalStake: Update total stake in StakingManager output
3. AdjustStakedAmount:
User deposit or withdraw INDY tokens to/from their staking profile.
4. Unstake: User remove their staking position
5. Lock: Lock an amount of INDY by voting in the Governance Contract
6. Unlock: Unlock locked INDY after voting period ends
-}
data StakingRedeemer
  = CreateStakingPosition Ledger.PaymentPubKeyHash
  | UpdateTotalStake
  | Distribute
  | AdjustStakedAmount Integer
  | Unstake
  | Lock
  | Unlock
  | UpgradeVersion
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''StakingRedeemer
PlutusTx.makeIsDataIndexed
  ''StakingRedeemer
  [ ('CreateStakingPosition, 0),
    ('UpdateTotalStake, 1),
    ('Distribute, 2),
    ('AdjustStakedAmount, 3),
    ('Unstake, 4),
    ('Lock, 5),
    ('Unlock, 6),
    ('UpgradeVersion, 7)
  ]

data Staking

instance TScripts.ValidatorTypes Staking where
  type DatumType Staking = StakingDatum
  type RedeemerType Staking = StakingRedeemer

{-# INLINEABLE removeExpiredLockedAmount #-}
removeExpiredLockedAmount ::
  Map Integer (Integer, Ledger.POSIXTime) ->
  (Ledger.POSIXTime -> Bool) ->
  Map Integer (Integer, Ledger.POSIXTime)
removeExpiredLockedAmount locked f =
  AssocMap.filter (\(_, time) -> f time) locked

{-# INLINEABLE getReward #-}
getReward :: RewardSnapshot -> RewardSnapshot -> Integer -> Ledger.Value
getReward oldSnapshot curSnapshot currentStake =
  Ada.lovelaceValueOf adaAmt
  where
    adaAmt =
      getOnChainInt $
        (snapshotAda curSnapshot - snapshotAda oldSnapshot)
          * OnChainDecimal currentStake

{-# INLINEABLE distributeReward #-}
distributeReward :: RewardSnapshot -> Integer -> Integer -> RewardSnapshot
distributeReward RewardSnapshot {..} adaReward totalStake =
  RewardSnapshot
    { snapshotAda =
        snapshotAda
          + OnChainDecimal adaReward
            `divide` OnChainDecimal totalStake
    }
