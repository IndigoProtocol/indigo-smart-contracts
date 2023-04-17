-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.Contracts.Governance.Poll.Common
  ( VoteOption (Yes, No),
    PollStatus (VoteCount, nYes, nNo),
    PollShard (PollShard, psId, psStatus, psEndTime, psManagerAddress),
    PollManager
      ( PollManager,
        pId,
        pOwner,
        pContent,
        pStatus,
        pEndTime,
        pCreatedShards,
        pTalliedShards,
        pTotalShards,
        pProposeEndTime,
        pExpirationTime,
        pProtocolVersion
      ),
    DistributionSchedule
      ( MkDistributionSchedule,
        λM_spd,
        z_spd,
        λM_lpd,
        z_lpd,
        λM_ipd,
        z_ipd,
        λM_tv,
        z_tv
      ),
    PollParams (..),
    PollManagerParams (..),
    PollRedeemer (Vote, MergeShards),
    PollManagerRedeemer (EndPoll, CreateShards, MergeShardsManager),
    PollScript,
    PollManagerScript,
    vote,
    pollPassQuorum,
    q,
    electorate,
    ve,
    vm,
    pollTokenName,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import Indigo.Contracts.Governance.Gov.Common (ProposalContent)
import Indigo.Data.Decimal qualified as OCD
import Indigo.Utils.Helpers qualified as Helpers
import Indigo.Utils.Spooky qualified as Spooky
import Ledger qualified
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Builtins (divideInteger)
import PlutusTx.Prelude
import PlutusTx.Sqrt (Sqrt (Approximately, Exactly, Imaginary), isqrt)
import Prelude qualified as P

pollTokenName :: Value.TokenName
pollTokenName = Value.TokenName "poll_token"

data VoteOption = Yes | No
  deriving (P.Show, Generic, ToJSON, FromJSON, P.Eq)

PlutusTx.makeLift ''VoteOption
PlutusTx.makeIsDataIndexed ''VoteOption [('Yes, 0), ('No, 1)]

data PollStatus = VoteCount {nYes :: Integer, nNo :: Integer}
  deriving (P.Show, Generic, ToJSON, FromJSON, P.Eq)

PlutusTx.makeLift ''PollStatus
PlutusTx.makeIsDataIndexed ''PollStatus [('VoteCount, 0)]

instance Eq PollStatus where
  {-# INLINEABLE (==) #-}
  (VoteCount y1 n1) == (VoteCount y2 n2) =
    y1 == y2 && n1 == n2

instance Semigroup PollStatus where
  {-# INLINEABLE (<>) #-}
  ps1 <> ps2 =
    VoteCount
      { nYes = nYes ps1 + nYes ps2,
        nNo = nNo ps1 + nNo ps2
      }

instance Monoid PollStatus where
  {-# INLINEABLE mempty #-}
  mempty = VoteCount {nYes = 0, nNo = 0}

data PollManager = PollManager
  { pId :: Integer,
    pOwner :: Ledger.PaymentPubKeyHash,
    pContent :: ProposalContent,
    pStatus :: PollStatus,
    pEndTime :: Ledger.POSIXTime,
    -- number of shards already created
    pCreatedShards :: Integer,
    -- number of shards merged into PollManager
    pTalliedShards :: Integer,
    -- total number of shards
    pTotalShards :: Integer,
    pProposeEndTime :: Ledger.POSIXTime,
    pExpirationTime :: Ledger.POSIXTime,
    pProtocolVersion :: Integer
  }

PlutusTx.makeLift ''PollManager
PlutusTx.makeIsDataIndexed ''PollManager [('PollManager, 0)]

data PollShard = PollShard
  { psId :: Integer,
    psStatus :: PollStatus,
    psEndTime :: Ledger.POSIXTime,
    -- | poll manager validator address
    psManagerAddress :: Spooky.Address
  }

PlutusTx.makeLift ''PollShard
PlutusTx.makeIsDataIndexed ''PollShard [('PollShard, 1)]

-- |  `λM` is the distribution map of the vesting schedule
--    `z` is the delay of a distribution schedule.
--    `λM` values are representing a map using the list since
--    all the values in the map have the same spaces.
data DistributionSchedule = MkDistributionSchedule
  { λM_spd :: [OCD.OnChainDecimal],
    z_spd :: Integer,
    λM_lpd :: [OCD.OnChainDecimal],
    z_lpd :: Integer,
    λM_ipd :: [OCD.OnChainDecimal],
    z_ipd :: Integer,
    λM_tv :: OCD.OnChainDecimal,
    z_tv :: Integer
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord, OpenApi.ToSchema)

PlutusTx.makeLift ''DistributionSchedule
PlutusTx.makeIsDataIndexed ''DistributionSchedule [('MkDistributionSchedule, 0)]

data PollParams = PollParams
  { pollToken :: Spooky.AssetClass,
    stakingToken :: Spooky.AssetClass,
    indyAsset :: Spooky.AssetClass,
    stakingValHash :: Spooky.ValidatorHash
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord, OpenApi.ToSchema)

PlutusTx.makeLift ''PollParams
PlutusTx.makeIsDataIndexed ''PollParams [('PollParams, 0)]

data PollManagerParams = PollManagerParams
  { govNFT :: Value.AssetClass,
    pollToken :: Value.AssetClass,
    upgradeToken :: Value.AssetClass,
    stakingToken :: Value.AssetClass,
    indyAsset :: Value.AssetClass,
    govExecuteValHash :: Ledger.ValidatorHash,
    stakingValHash :: Ledger.ValidatorHash,
    pBiasTime :: Ledger.POSIXTime,
    -- | poll shards validator hash
    shardsValHash :: Ledger.ValidatorHash,
    treasuryValHash :: Ledger.ValidatorHash,
    -- | This is the ITD for the electorate calculation.
    initialIndyDistribution :: Integer,
    -- | This is the t parameter in the electorate calculation.
    totalINDYSupply :: Integer,
    -- | distribution maps for each vesting schedule
    distributionSchedule :: DistributionSchedule
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord, OpenApi.ToSchema)

PlutusTx.makeLift ''PollManagerParams
PlutusTx.makeIsDataIndexed ''PollManagerParams [('PollManagerParams, 0)]

data PollRedeemer
  = Vote VoteOption
  | MergeShards Ledger.POSIXTime Spooky.TxOutRef

PlutusTx.makeLift ''PollRedeemer
PlutusTx.makeIsDataIndexed ''PollRedeemer [('Vote, 0), ('MergeShards, 1)]

data PollManagerRedeemer
  = EndPoll Ledger.POSIXTime
  | CreateShards Ledger.POSIXTime
  | MergeShardsManager Ledger.POSIXTime

PlutusTx.makeLift ''PollManagerRedeemer
PlutusTx.makeIsDataIndexed
  ''PollManagerRedeemer
  [('EndPoll, 0), ('CreateShards, 1), ('MergeShardsManager, 2)]

{-# INLINEABLE vote #-}
vote :: PollShard -> VoteOption -> Integer -> PollShard
vote poll@PollShard {psStatus} Yes amt =
  poll {psStatus = psStatus {nYes = nYes psStatus + amt}}
vote poll@PollShard {psStatus} No amt =
  poll {psStatus = psStatus {nNo = nNo psStatus + amt}}

-- | Check for the quorum pass/fail.
--   The 'q' calculation is not accurate since it uses fixed number of decimals.
--   There can be multiple discrepancies.
--   One of them happens when the `q` is around 0.
--   This happens when the votes are the same or nearly the same.
--   In these cases the quorum nondeterministically passes/fails.
--   Because of this, the threshold for the quorum to pass
--   is set to be slightly greater than 0.
--
--   The semantics of this value are that the `q` is biased towards the failing
--   in case of yes/no similarities in voting.
--   Additionaly, this tells that a higher significance of yes votes is expected
--   to pass the quorum.
{-# INLINEABLE pollPassQuorum #-}
pollPassQuorum ::
  PollStatus ->
  Ledger.POSIXTime ->
  Integer ->
  Integer ->
  Ledger.POSIXTime ->
  DistributionSchedule ->
  Bool
pollPassQuorum
  pStatus
  protocolStartTime
  indyAtLaunch
  indyTotalSupply
  currentTime
  distributionSchedule =
    q
      pStatus
      protocolStartTime
      indyAtLaunch
      indyTotalSupply
      currentTime
      distributionSchedule
      > OCD.OnChainDecimal 50_000 -- 0.05

-- |  Calculates the core equation for the adaptive quorum.
--    Ref: https://drive.google.com/uc?id=12K0tzNAy0Ef1OQBRE6unofAH-szIVB1a
{-# INLINEABLE q #-}
q ::
  PollStatus ->
  Ledger.POSIXTime ->
  Integer ->
  Integer ->
  Ledger.POSIXTime ->
  DistributionSchedule ->
  OCD.OnChainDecimal
q
  VoteCount {nYes, nNo}
  protocolStartTime
  indyAtLaunch
  indyTotalSupply
  currentTime
  distributionSchedule =
    if nYes + nNo == 0
      then 0
      else
        let e =
              electorate
                indyAtLaunch
                indyTotalSupply
                protocolStartTime
                currentTime
                distributionSchedule
         in ( OCD.OnChainDecimal (nYes * OCD.decimalUnit)
                `OCD.divide` sqrtRes (OCD.OnChainDecimal (e * OCD.decimalUnit))
            )
              - ( OCD.OnChainDecimal (nNo * OCD.decimalUnit)
                    `OCD.divide` sqrtRes
                      (OCD.OnChainDecimal ((nYes + nNo) * OCD.decimalUnit))
                )
    where
      sqrtRes :: OCD.OnChainDecimal -> OCD.OnChainDecimal
      sqrtRes (OCD.OnChainDecimal a) = case isqrt a of
        Imaginary -> error ()
        Exactly n -> OCD.OnChainDecimal (n * 1_000)
        Approximately n -> OCD.OnChainDecimal (n * 1_000)

-- |  The electorate calculation.
--    Ref: https://drive.google.com/uc?id=12K0tzNAy0Ef1OQBRE6unofAH-szIVB1a
{-# INLINEABLE electorate #-}
electorate ::
  -- | amount of INDY distributed at protocol launch
  Integer ->
  -- | INDY total supply
  Integer ->
  -- | Date of the first epoch after protocol launch
  Ledger.POSIXTime ->
  -- | The date to determine the circulating for
  Ledger.POSIXTime ->
  DistributionSchedule ->
  Integer
electorate itd t p d distributionSchedule =
  let ve' = ve t p d
      vm' = vm t p d
   in divideInteger itd OCD.decimalUnit
        + ve' (z_spd distributionSchedule) (λM_spd distributionSchedule)
        + ve' (z_lpd distributionSchedule) (λM_lpd distributionSchedule)
        + ve' (z_ipd distributionSchedule) (λM_ipd distributionSchedule)
        + vm' (z_tv distributionSchedule) (λM_tv distributionSchedule)

-- | This is used in the v_e and v_m calculations as the part of
--   the electorate calculation.
--   Ref: https://drive.google.com/uc?id=12K0tzNAy0Ef1OQBRE6unofAH-szIVB1a
{-# INLINEABLE l #-}
l :: Integer -> [OCD.OnChainDecimal] -> Integer -> Integer
l t λM x =
  OCD.decimal2Integer
    ( (OCD.OnChainDecimal t * sum λM)
        - OCD.OnChainDecimal
          ( OCD.decimalUnit
              * ( x
                    * foldl
                      ( \ !acc a ->
                          acc
                            + OCD.decimal2Integer
                              ( (OCD.OnChainDecimal t * a)
                                  `OCD.divide` OCD.OnChainDecimal
                                    (OCD.decimalUnit * x)
                              )
                      )
                      0
                      λM
                )
          ) -- necessary rational calculation
    )

-- | This is used to calculate electorate.
--   The original equation for this calculation includes
--   a sum that can go maximally 365 iterations based on
--   the number of days from protocol launch.
{-# INLINEABLE ve #-}
ve ::
  Integer ->
  Ledger.POSIXTime ->
  Ledger.POSIXTime ->
  Integer ->
  [OCD.OnChainDecimal] ->
  Integer
ve t p d z λM =
  let lRes :: Integer
      lRes = l t λM 73
      λe :: Integer
      λe = min ((Helpers.daysDifference d p `divide` 5) - z + 1) 365
      -- Group ie calculation replaces the need to iteratively sum
      -- all the ie calculations individually in the ve calculation.
      --
      -- Since iterations differ in the lookup to λM, there can be 5 groups
      -- that can be simply grouped together and calculated at once
      -- without the need to iterate.
      groupIeCalculation :: OCD.OnChainDecimal -> Integer -> Integer -> Integer
      groupIeCalculation (OCD.OnChainDecimal lookupRes) count minN =
        let -- necessary rational calculation
            v =
              OCD.decimal2Integer
                ( OCD.OnChainDecimal t
                    * ( OCD.OnChainDecimal (lookupRes * OCD.decimalUnit)
                          `OCD.divide` 73
                      )
                )
                `divideInteger` OCD.decimalUnit
            rangeIncludesL = lRes >= minN && minN + count - 1 >= lRes
         in if rangeIncludesL
              then
                let lowerCount = lRes - minN
                 in -- chunks range and calculates separately for those that are
                    -- under the l and those that are above and equal
                    lowerCount * (v + 1) + (count - lowerCount) * v
              else
                if minN >= lRes
                  then count * v
                  else count * (v + 1)
      -- Whole group is 73 iterations.
      -- These can be calculated in one go using the same lookup,
      -- without the need to iterate 73 times for the group.
      groupsSumCalc :: Integer -> Integer
      groupsSumCalc count
        | count <= 0 = 0
        | otherwise =
            groupIeCalculation
              (λM !! (count - 1))
              73
              ((count - 1) * 73)
              + groupsSumCalc (count - 1)
   in if λe < 0
        then 0
        else
          let (x, rest) = λe `divMod` 73
           in groupsSumCalc x
                + if rest > 0
                  then groupIeCalculation (λM !! x) rest (x * 73)
                  else 0

-- | This is used to calculate electorate.
--   The original equation for this calculation includes a sum that can go
--   maximally 24 iterations based on number of days from protocol launch.
{-# INLINEABLE vm #-}
vm ::
  Integer ->
  Ledger.POSIXTime ->
  Ledger.POSIXTime ->
  Integer ->
  OCD.OnChainDecimal ->
  Integer
vm t p d z (OCD.OnChainDecimal λM) =
  let monthLength :: OCD.OnChainDecimal
      monthLength = OCD.OnChainDecimal 30_417_000 -- 365 / 12
      dpDiff :: Integer
      dpDiff = Helpers.daysDifference d p
      λm :: Integer
      λm =
        min
          ( OCD.decimal2Integer
              ( OCD.OnChainDecimal (OCD.decimalUnit * (dpDiff - z))
                  `OCD.divide` monthLength
              )
              + 1
          )
          24 -- necessary rational calculation
   in if λm < 0
        then 0
        else
          let -- necessary rational calculation
              qX :: OCD.OnChainDecimal
              qX =
                OCD.OnChainDecimal t
                  * (OCD.OnChainDecimal (λM * OCD.decimalUnit) `OCD.divide` 24)
           in if λm == 0 && dpDiff >= 0
                then OCD.decimal2Integer qX `divideInteger` OCD.decimalUnit
                else
                  OCD.decimal2Integer
                    (OCD.OnChainDecimal (λm * OCD.decimalUnit) * qX)
                    `divideInteger` OCD.decimalUnit

data PollScript

data PollManagerScript

instance TScripts.ValidatorTypes PollScript where
  type DatumType PollScript = PollShard
  type RedeemerType PollScript = PollRedeemer

instance TScripts.ValidatorTypes PollManagerScript where
  type DatumType PollManagerScript = PollManager
  type RedeemerType PollManagerScript = PollManagerRedeemer
