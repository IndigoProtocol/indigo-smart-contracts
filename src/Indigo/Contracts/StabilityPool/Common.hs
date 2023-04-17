-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.Contracts.StabilityPool.Common
  ( SPInteger (),
    (|-|),
    (|+|),
    (|*|),
    (|/|),
    spTruncate,
    stabilityPoolTokenName,
    snapshotEpochToScaleToSumTokenName,
    accountTokenName,
    StabilityPoolParams
      ( StabilityPoolParams,
        assetSymbol,
        stabilityPoolToken,
        snapshotEpochToScaleToSumToken,
        accountToken,
        cdpToken,
        versionRecordToken,
        collectorValHash,
        govNFT,
        accountCreateFeeLovelaces,
        accountAdjustmentFeeLovelaces
      ),
    StabilityPoolSnapshot
      ( StabilityPoolSnapshot,
        snapshotP,
        snapshotD,
        snapshotS,
        snapshotEpoch,
        snapshotScale
      ),
    StabilityDatum
      ( StabilityPoolDatum,
        spIAsset,
        spSnapshot,
        epochToScaleToSum,
        AccountDatum,
        accOwner,
        accIAsset,
        accSnapshot,
        SnapshotEpochToScaleToSumDatum,
        sessSnapshot,
        sessAsset
      ),
    StabilityPoolRedeemer
      ( CreateAccount,
        caPkh,
        caAmount,
        AdjustAccount,
        aaDepositChange,
        LiquidateCDP,
        Close,
        SpendAccount,
        UpgradeVersion,
        RecordEpochToScaleToSum
      ),
    StabilityPoolScript,
    EpochToScaleToSum,
    initSPSnapshot,
    initEpochToScaleToSumMap,
    toSPInteger,
    fromSPInteger,
    fromSPIntegerToOCD,
    adjustAccountHelper,
    liquidateHelper,
    partitionEpochToScaleToSumMap,
    snapshotPLens,
    snapshotDLens,
    snapshotSLens,
    snapshotEpochLens,
    snapshotScaleLens,
  )
where

import Control.Lens (makeLensesFor)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import Indigo.Data.Decimal
import Ledger qualified
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (ValidatorHash)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (divideInteger, multiplyInteger)
import PlutusTx.Prelude hiding (divide)
import Prelude qualified as P

stabilityPoolTokenName :: Value.TokenName
stabilityPoolTokenName = Value.TokenName "stability_pool_token"

snapshotEpochToScaleToSumTokenName :: Value.TokenName
snapshotEpochToScaleToSumTokenName = Value.TokenName "snapshot_token"

accountTokenName :: Value.TokenName
accountTokenName = Value.TokenName "account_token"

data StabilityPoolParams = StabilityPoolParams
  { assetSymbol :: Value.CurrencySymbol,
    stabilityPoolToken :: Value.AssetClass,
    snapshotEpochToScaleToSumToken :: Value.AssetClass,
    accountToken :: Value.AssetClass,
    cdpToken :: Value.AssetClass,
    versionRecordToken :: Value.AssetClass,
    collectorValHash :: ValidatorHash,
    govNFT :: Value.AssetClass,
    accountCreateFeeLovelaces :: Integer,
    accountAdjustmentFeeLovelaces :: Integer
  }
  deriving (Generic, P.Show, P.Ord, ToJSON, FromJSON, P.Eq, OpenApi.ToSchema)

PlutusTx.makeLift ''StabilityPoolParams
PlutusTx.makeIsDataIndexed ''StabilityPoolParams [('StabilityPoolParams, 0)]

-- | This type is used to represent numbers at the stability pool calculations.
-- It wraps an Integer that represents a decimal with custom precision.
newtype SPInteger = SPInteger {getSPInteger :: Integer}
  deriving stock (P.Show, P.Eq, P.Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''SPInteger
PlutusTx.makeIsDataIndexed ''SPInteger [('SPInteger, 0)]

spPrecision :: Integer
spPrecision = 1_000_000_000_000_000_000

-- | Multiplication with custom precision
(|*|) :: SPInteger -> SPInteger -> SPInteger
(|*|) (SPInteger a) (SPInteger b) =
  SPInteger $ divideInteger (multiplyInteger a b) spPrecision

-- | Division with custom precision
(|/|) :: SPInteger -> SPInteger -> SPInteger
(|/|) (SPInteger a) (SPInteger b) =
  SPInteger $ divideInteger (multiplyInteger a spPrecision) b

-- | Subtraction with custom precision
(|-|) :: SPInteger -> SPInteger -> SPInteger
(|-|) (SPInteger a) (SPInteger b) = SPInteger $ a - b

-- | Addition with custom precision
(|+|) :: SPInteger -> SPInteger -> SPInteger
(|+|) (SPInteger a) (SPInteger b) = SPInteger $ a + b

-- | Converts from custom precision Integer to Integer
fromSPInteger :: SPInteger -> Integer
fromSPInteger (SPInteger a) = divideInteger a spPrecision

toSPInteger :: Integer -> SPInteger
toSPInteger a = SPInteger $ multiplyInteger a spPrecision

fromSPIntegerToOCD :: SPInteger -> OnChainDecimal
fromSPIntegerToOCD (SPInteger a) =
  OnChainDecimal $ divideInteger (multiplyInteger a decimalUnit) spPrecision

spTruncate :: SPInteger -> SPInteger
spTruncate (SPInteger a)
  | a > 0 = SPInteger {getSPInteger = a}
  | otherwise = SPInteger {getSPInteger = 0}

data StabilityPoolSnapshot = StabilityPoolSnapshot
  { -- | Product snapshot - intermediate variable.
    -- Reference to product snapshot can be found
    -- in https://github.com/liquity/dev#how-deposits-and-eth-gains-are-tracked
    snapshotP :: SPInteger,
    -- | Deposit snapshot
    snapshotD :: SPInteger,
    -- | Sum snapshot - intermediate variable.
    -- Reference to sum snapshot can be found
    -- in https://github.com/liquity/dev#how-deposits-and-eth-gains-are-tracked
    snapshotS :: SPInteger,
    -- | Tracks pool emptying events.
    -- When a pool is emptied the epoch is incremented.
    snapshotEpoch :: Integer,
    -- | This is used to prevent change of `snapshotP` to 0.
    --
    -- From Liquity paper
    -- https://github.com/liquity/dev/blob/main/papers/Scalable_Reward_Distribution_with_Compounding_Stakes.pdf :
    -- * Upon a liquidation that would otherwise truncate P to 0, S is first
    -- updated as usual, for the current scale. Then, the current scale is
    -- incremented by 1, and P is updated and scaled by 1e18.
    snapshotScale :: Integer
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord)

PlutusTx.makeLift ''StabilityPoolSnapshot
PlutusTx.makeIsDataIndexed ''StabilityPoolSnapshot [('StabilityPoolSnapshot, 0)]
makeLensesFor
  [ ("snapshotP", "snapshotPLens"),
    ("snapshotD", "snapshotDLens"),
    ("snapshotS", "snapshotSLens"),
    ("snapshotEpoch", "snapshotEpochLens"),
    ("snapshotScale", "snapshotScaleLens")
  ]
  ''StabilityPoolSnapshot

type EpochToScaleToSum = AssocMap.Map (Integer, Integer) SPInteger

data StabilityDatum
  = StabilityPoolDatum
      { spIAsset :: Value.TokenName,
        spSnapshot :: StabilityPoolSnapshot,
        epochToScaleToSum :: EpochToScaleToSum
      }
  | AccountDatum
      { accOwner :: Ledger.PaymentPubKeyHash,
        accIAsset :: Value.TokenName,
        accSnapshot :: StabilityPoolSnapshot
      }
  | SnapshotEpochToScaleToSumDatum
      { sessSnapshot :: EpochToScaleToSum,
        sessAsset :: Value.TokenName
      }
  deriving stock (P.Show, P.Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''StabilityDatum
PlutusTx.makeIsDataIndexed
  ''StabilityDatum
  [ ('StabilityPoolDatum, 0),
    ('AccountDatum, 1),
    ('SnapshotEpochToScaleToSumDatum, 2)
  ]

data StabilityPoolRedeemer
  = CreateAccount {caPkh :: Ledger.PaymentPubKeyHash, caAmount :: Integer}
  | AdjustAccount
      { -- | The deposit change amount, it is negative in case of withdrawal
        -- and positive in case of deposit
        aaDepositChange :: Integer
      }
  | LiquidateCDP
  | Close
  | SpendAccount
  | RecordEpochToScaleToSum
  | UpgradeVersion
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''StabilityPoolRedeemer
PlutusTx.makeIsDataIndexed
  ''StabilityPoolRedeemer
  [ ('CreateAccount, 0),
    ('AdjustAccount, 1),
    ('LiquidateCDP, 2),
    ('Close, 3),
    ('SpendAccount, 4),
    ('UpgradeVersion, 5),
    ('RecordEpochToScaleToSum, 6)
  ]

data StabilityPoolScript

instance TScripts.ValidatorTypes StabilityPoolScript where
  type DatumType StabilityPoolScript = StabilityDatum
  type RedeemerType StabilityPoolScript = StabilityPoolRedeemer

newScaleMultiplier :: Integer
newScaleMultiplier = 1_000_000_000

initSPSnapshot :: StabilityPoolSnapshot
initSPSnapshot =
  StabilityPoolSnapshot
    { snapshotP = toSPInteger 1,
      snapshotD = SPInteger 0,
      snapshotS = SPInteger 0,
      snapshotEpoch = 0,
      snapshotScale = 0
    }

initEpochToScaleToSumMap :: EpochToScaleToSum
initEpochToScaleToSumMap = AssocMap.fromList [((0, 0), SPInteger 0)]

-- | Calculate the updated account's snapshot
-- and account's reward in Ada (newSnapshot, reward).
--
-- When an epoch changed (difference between epoch of pool snapshot and
-- the account's one), the pool was emptied, meaning that all the deposits
-- were depleted, so all the deposits (`snapshotD`) that existed
-- before that epoch are now 0.
--
-- The use of `EpochToScaleToSum` storage in the `accountReward` calculation
-- is explained in the Liquity paper
-- https://github.com/liquity/dev/blob/main/papers/Scalable_Reward_Distribution_with_Compounding_Stakes.pdf :
--
-- * /ETH Gain/: Since the deposit may span up to one scale change,
-- so too does the reward. In this case, obtain the ETH gain using the S sums
-- from the two consecutive scales that the deposit spans.
-- The reward from the second sum is scaled by 1e-18. The rewards from both
-- scales are added to make the final ETH gain.
--
-- Medium article with more details:
-- https://medium.com/liquity/scaling-liquitys-stability-pool-c4c6572cf275
{-# INLINEABLE adjustAccountHelper #-}
adjustAccountHelper ::
  StabilityPoolSnapshot ->
  StabilityPoolSnapshot ->
  EpochToScaleToSum ->
  (StabilityPoolSnapshot, Maybe Integer)
adjustAccountHelper pool account snapshotSMap =
  ( pool
      { snapshotD =
          -- This check is covering this one from liquity
          -- https://github.com/liquity/dev/blob/c4c5b9b5234b42ce4dd563095a42dcec6cabc452/packages/contracts/contracts/StabilityPool.sol#L817
          if getSPInteger fund
            < getSPInteger (snapshotD account |/| toSPInteger 1_000_000_000)
            then SPInteger 0
            else fund
      },
    fromSPInteger <$> accountReward
  )
  where
    fund :: SPInteger
    fund
      | snapshotEpoch pool > snapshotEpoch account = SPInteger 0
      | snapshotScale pool - snapshotScale account > 1 = SPInteger 0
      | snapshotScale pool > snapshotScale account =
          (snapshotD account |*| snapshotP pool)
            |/| SPInteger
              ( getSPInteger (snapshotP account)
                  * newScaleMultiplier
              )
      | otherwise = (snapshotD account |*| snapshotP pool) |/| snapshotP account

    accountReward :: Maybe SPInteger
    accountReward = do
      s1 <-
        AssocMap.lookup
          (snapshotEpoch account, snapshotScale account)
          snapshotSMap
      let s2 =
            fromMaybe s1 $
              AssocMap.lookup
                (snapshotEpoch account, snapshotScale account + 1)
                snapshotSMap
          a1 = s1 |-| snapshotS account
          a2 = (s2 |-| s1) |/| toSPInteger newScaleMultiplier
      Just (((a1 |+| a2) |*| snapshotD account) |/| snapshotP account)

-- | Helper for CDP liquidation. Returns the new StabilityPool's datum snapshot
-- and epochToScaleToSum (updated based on the liquidation).
-- https://github.com/liquity/dev/blob/main/papers/Scalable_Reward_Distribution_with_Compounding_Stakes.pdf
--
-- ### EpochToScaleToSum details
--
-- The `newEpochToScaleToSum` stores the last sum (`snapshotS`).
--
-- In case the liquidation empties the pool the `newEpochToScaleToSum` has a
-- special behaviour where it stores the sum for the current epoch and then
-- increases the epoch by 1. Check out the following note from the Liquity paper
-- https://github.com/liquity/dev/blob/main/papers/Scalable_Reward_Distribution_with_Compounding_Stakes.pdf
--
--
-- * /Liquidation that empties the Pool:/
-- Compute the latest value of S, and store it for the current epoch.
-- Increase the epoch by 1, and reset the product and sum (P = 1, S = 0).
--
-- A similar note but for the scale:
--
-- * /Liquidation:/
-- Compute the latest value of S, store it for the current scale
-- and increment the current scale by 1.
{-# INLINEABLE liquidateHelper #-}
liquidateHelper ::
  StabilityPoolSnapshot ->
  EpochToScaleToSum ->
  Integer ->
  Integer ->
  (StabilityPoolSnapshot, EpochToScaleToSum)
liquidateHelper
  StabilityPoolSnapshot
    { snapshotD,
      snapshotP,
      snapshotS,
      snapshotEpoch,
      snapshotScale
    }
  epochToScaleToSum
  burn'
  reward' =
    (liquidateSnapshot, newEpochToScaleToSum)
    where
      {- Epoch increases every time the stability pool is emptied or the running product
         gets truncated to 0 because of being smaller than what can be represented with
         the fixed precision used, even after multiplying it by the scale factor.

         This is to avoid having the running product equal to 0.

         It's possible that the snapshotP can get below 0 due to rounding issues. In
         that case we increase the epoch.
      -}
      isEpochIncrease :: Bool
      isEpochIncrease = getSPInteger newSnapshotP <= 0

      {- Scale increases every time the running product gets below a threshold.

         This is to avoid unnecessarily increasing the epoch.
      -}
      isScaleIncrease :: Bool
      isScaleIncrease = getSPInteger (snapshotP |*| productFactor) < newScaleMultiplier

      burn :: SPInteger
      burn = toSPInteger burn'

      reward :: SPInteger
      reward = toSPInteger reward'

      newSnapshotS :: SPInteger
      newSnapshotS = snapshotS |+| ((reward |*| snapshotP) |/| snapshotD)

      newSnapshotP :: SPInteger
      newSnapshotP =
        SPInteger
          ( (if isScaleIncrease then newScaleMultiplier else 1)
              * getSPInteger snapshotP
          )
          |*| productFactor

      productFactor :: SPInteger
      productFactor =
        let lossPerUnitStaked = burn |/| snapshotD
         in toSPInteger 1 |-| lossPerUnitStaked

      liquidateSnapshot :: StabilityPoolSnapshot
      liquidateSnapshot =
        if isEpochIncrease
          then initSPSnapshot {snapshotEpoch = snapshotEpoch + 1}
          else
            StabilityPoolSnapshot
              { snapshotP = newSnapshotP,
                snapshotD = newSnapshotD,
                snapshotS = newSnapshotS,
                snapshotEpoch = snapshotEpoch,
                snapshotScale = newScale
              }
        where
          newScale = snapshotScale + if isScaleIncrease then 1 else 0
          newSnapshotD = snapshotD |-| burn

      newEpochToScaleToSum :: EpochToScaleToSum
      newEpochToScaleToSum
        | isEpochIncrease =
            AssocMap.insert (snapshotEpoch + 1, 0) (toSPInteger 0) newMap
        | isScaleIncrease =
            AssocMap.insert
              (snapshotEpoch, snapshotScale + 1)
              newSnapshotS
              newMap
        | otherwise = newMap
        where
          newMap =
            AssocMap.insert
              (snapshotEpoch, snapshotScale)
              newSnapshotS
              epochToScaleToSum

-- | This helpers is used for partitioning the epochToScaleToSum map
-- for the purpose of recording epochToScaleToSum map.
{-# INLINEABLE partitionEpochToScaleToSumMap #-}
partitionEpochToScaleToSumMap ::
  StabilityPoolSnapshot ->
  EpochToScaleToSum ->
  (((Integer, Integer), SPInteger), [((Integer, Integer), SPInteger)])
partitionEpochToScaleToSumMap
  StabilityPoolSnapshot {snapshotEpoch, snapshotScale}
  epochToScaleToSum =
    let ([remainingMapItem], snapshotMapItems) =
          partition
            ( \((epoch', scale'), _) ->
                snapshotEpoch == epoch' && snapshotScale == scale'
            )
            (AssocMap.toList epochToScaleToSum)
     in (remainingMapItem, snapshotMapItems)
