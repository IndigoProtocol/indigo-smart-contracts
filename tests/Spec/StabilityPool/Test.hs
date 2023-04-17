{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Spec.StabilityPool.Test (tests) where

import Control.Arrow (Arrow (second))
import Control.Lens (Bifunctor (bimap), over)
import Control.Monad.State.Strict (State, execState, get, put)
import Data.Map qualified as Map
import Indigo.Contracts.StabilityPool.Common
  ( adjustAccountHelper,
    liquidateHelper,
  )
import Indigo.Contracts.StabilityPool.Common hiding
  ( StabilityPoolScript,
    adjustAccountHelper,
    epochToScaleToSum,
    liquidateHelper,
  )
import Indigo.Data.Decimal
  ( OnChainDecimal (OnChainDecimal, getOnChainInt),
    decimalUnit,
  )
import Options (SuiteOptions, setupTestBenchmarkSuite)
import PlutusTx.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Prelude qualified as P

epsilon :: Integer
epsilon = 6

compareWithEps ::
  (HasCallStack, P.Integral a, Ord a, P.Show a) => a -> a -> Assertion
compareWithEps actual expected =
  if P.toInteger (actual P.- expected) <= epsilon
    then P.pure ()
    else
      P.error
        ( "\nExpected: "
            <> P.show expected
            <> "\nActual: "
            <> P.show actual
        )

type UserUid = Integer

data SPState = SPState
  { sPoolSnapshot :: StabilityPoolSnapshot,
    epochToScaleToSum :: EpochToScaleToSum,
    accountsSnapshotsWithRewards ::
      Map.Map UserUid (StabilityPoolSnapshot, Integer)
  }
  deriving (P.Show)

tests :: SuiteOptions -> TestTree
tests suiteOpts =
  testGroup
    "Stability pool snapshot tests"
    ( setupTestBenchmarkSuite
        suiteOpts
        [ testCase "Example scenario from readme" exampleReadmeScenario,
          testCase
            "Depositors with equal initial deposit withdraw correct compounded\
            \ deposit and ADA Gain after one liquidation"
            t2,
          testCase
            "Depositors with equal initial deposit withdraw correct compounded\
            \ deposit and ADA Gain after two identical liquidations"
            t3,
          testCase
            "Depositors with equal initial deposit withdraw correct compounded\
            \ deposit and ADA Gain after three identical liquidations"
            t4,
          testCase
            "Depositors with equal initial deposit withdraw correct compounded\
            \ deposit and ADA Gain after two liquidations of increasing iAsset"
            t5,
          testCase
            "Depositors with equal initial deposit withdraw correct compounded\
            \ deposit and ADA Gain after three liquidations\
            \ of increasing iAsset"
            t6,
          testCase
            "Depositors with varying deposits withdraw correct compounded\
            \ deposit and ADA Gain after two identical liquidations"
            t7,
          testCase
            "Depositors with varying deposits withdraw correct compounded\
            \ deposit and ADA Gain after three identical liquidations"
            t8,
          testCase
            "Depositors with varying deposits withdraw correct compounded\
            \ deposit and ADA Gain after three varying liquidations"
            t9,
          testCase
            "A, B, C Deposit -> 2 liquidations -> D deposits -> 1 liquidation.\
            \ All deposits and liquidations = 100 iAsset.  A, B, C, D withdraw\
            \ correct iAsset deposit and ADA Gain"
            t10,
          testCase
            "Pool-emptying liquidation increases epoch by one, resets\
            \ scaleFactor to 0, and resets P to 1e18"
            t11,
          testCase "Deposit spans one scale factor change" t12,
          testCase
            "Several deposits of varying amounts span one scale factor change"
            t13,
          testCase
            "Several deposits of 10000 iAsset span 2 scale factor change"
            t14,
          testCase
            "Depositors withdraw correct compounded deposit after\
            \ liquidation empties the pool"
            t15,
          testCase
            "Small liquidated coll/debt, large deposits and ADA price"
            t16,
          testCase "Checking rewards over multiple scales" t17,
          testCase
            "Deposit decreases below 1e9th of the initial deposit,\
            \deposit becomes 0"
            t18,
          testCase "New test case" tNewCase
        ]
        []
    )

initState :: SPState
initState =
  SPState
    { sPoolSnapshot = initSPSnapshot,
      epochToScaleToSum = initEpochToScaleToSumMap,
      accountsSnapshotsWithRewards = Map.empty
    }

liquidate :: Integer -> Integer -> State SPState ()
liquidate burn reward = do
  st@SPState {sPoolSnapshot, epochToScaleToSum} <- get
  let (newSPSnapshot, newEpochToScaleToSum) = liquidateHelper sPoolSnapshot epochToScaleToSum burn reward
  put (st {sPoolSnapshot = newSPSnapshot, epochToScaleToSum = newEpochToScaleToSum})

accountAdjust :: HasCallStack => (Integer, OnChainDecimal) -> State SPState ()
accountAdjust (userId, toSPInteger . getOnChainInt -> depositAdjustment) = do
  st@SPState {sPoolSnapshot, epochToScaleToSum, accountsSnapshotsWithRewards} <- get
  put
    st
      { sPoolSnapshot =
          sPoolSnapshot
            { snapshotD = spTruncate (snapshotD sPoolSnapshot |+| depositAdjustment)
            },
        accountsSnapshotsWithRewards =
          Map.insertWith
            ( \_ (snap, reward) ->
                bimap
                  (over snapshotDLens (|+| depositAdjustment))
                  (\(Just a) -> a + reward)
                  (adjustAccountHelper sPoolSnapshot snap epochToScaleToSum)
            )
            userId
            (sPoolSnapshot {snapshotD = depositAdjustment}, 0)
            accountsSnapshotsWithRewards
      }

spDeposits :: [(Integer, OnChainDecimal)] -> State SPState ()
spDeposits newDeposits = do
  SPState {accountsSnapshotsWithRewards} <- get
  if Map.size accountsSnapshotsWithRewards P.== 0
    then mapM_ accountAdjust newDeposits
    else mapM_ accountAdjust (filter (\(_, deposit) -> deposit /= 0) newDeposits)

close :: HasCallStack => Integer -> State SPState ()
close userUid = do
  s@SPState {sPoolSnapshot, epochToScaleToSum, accountsSnapshotsWithRewards} <- get
  let Just (snap, rew) = Map.lookup userUid accountsSnapshotsWithRewards
      (newAccountSnapshot, Just reward) =
        adjustAccountHelper
          sPoolSnapshot
          snap
          epochToScaleToSum
      sPoolSnapshot' = over snapshotDLens (|-| snapshotD newAccountSnapshot) sPoolSnapshot
  put
    s
      { sPoolSnapshot = sPoolSnapshot' {snapshotD = spTruncate (snapshotD sPoolSnapshot')},
        accountsSnapshotsWithRewards =
          Map.insert
            userUid
            (newAccountSnapshot {snapshotD = toSPInteger 0}, rew + reward)
            accountsSnapshotsWithRewards
      }

-- | This is used to update reward but to let accounts unchanged so we can make assertions.
allClaimRewards :: HasCallStack => State SPState ()
allClaimRewards = do
  s@SPState {sPoolSnapshot, epochToScaleToSum, accountsSnapshotsWithRewards} <- get
  put
    s
      { accountsSnapshotsWithRewards =
          Map.map
            ( \(snap, reward') ->
                second
                  (\(Just a) -> a + reward')
                  (adjustAccountHelper sPoolSnapshot snap epochToScaleToSum)
            )
            accountsSnapshotsWithRewards
      }

-- | This mirrors the example from Liquity's readme:
-- https://github.com/liquity/dev#stability-pool-example
exampleReadmeScenario :: Assertion
exampleReadmeScenario = afterT1Liquidation >> afterT2Liquidation
  where
    afterT1Liquidation :: Assertion
    afterT1Liquidation =
      let SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC)])} =
            execState
              (spDeposits (zip [0 ..] [100, 200, 300]) >> liquidationT1 >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt 75)
            >> ( accRewardA
                   `compareWithEps` getOnChainInt (OnChainDecimal 26_666_666)
               )
            >> ( fromSPInteger (snapshotD accSnapB)
                   `compareWithEps` getOnChainInt 150
               )
            >> ( accRewardB
                   `compareWithEps` getOnChainInt (OnChainDecimal 53_333_333)
               )
            >> ( fromSPInteger (snapshotD accSnapC)
                   `compareWithEps` getOnChainInt 225
               )
            >> (accRewardC `compareWithEps` getOnChainInt 80)

    afterT2Liquidation :: Assertion
    afterT2Liquidation =
      let SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC)])} =
            execState
              (spDeposits (zip [0 ..] [100, 200, 300]) >> liquidationT1 >> liquidationT2 >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt (OnChainDecimal 37_500_000))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 67_500_000))
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt 75)
            >> (accRewardB `compareWithEps` getOnChainInt 135)
            >> ( fromSPInteger (snapshotD accSnapC)
                   `compareWithEps` getOnChainInt (OnChainDecimal 112_500_000)
               )
            >> ( accRewardC
                   `compareWithEps` getOnChainInt (OnChainDecimal 202_500_000)
               )

    liquidationT1 :: State SPState ()
    liquidationT1 = liquidate (150 * decimalUnit) (160 * decimalUnit)

    liquidationT2 :: State SPState ()
    liquidationT2 = liquidate (225 * decimalUnit) (245 * decimalUnit)

-- | Depositors with equal initial deposit withdraw correct compounded deposit
-- and ADA Gain after one liquidation
-- 1. 3 Identical SP Deposits occur of 10k
-- 2. A liquidation of a CDP with 10k iAsset and 100 ADA occurs.
-- 3. Each depositor expects to have 6666.666666 iAsset available
-- with 33.333333 ADA available as reward,
t2 :: Assertion
t2 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC)])} =
            execState
              (spDeposits (zip [0 ..] [10_000, 10_000, 10_000]) >> liquidation >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt (OnChainDecimal 6_666_666_666))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 33_333_333))
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt (OnChainDecimal 6_666_666_666))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 33_333_333))
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 6_666_666_666))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 33_333_333))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 666_666)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation :: State SPState ()
    liquidation = liquidate (10_000 * decimalUnit) (100 * decimalUnit)

-- | Depositors with equal initial deposit withdraw correct compounded deposit
-- and ADA Gain after two identical liquidations
-- 1. 3 Identical SP Deposits occur of 10k
-- 2. Two equal liquidations of a CDP with 10k iAsset and 100 ADA occurs.
-- 3. Each depositor expects to have 3333.333333 iAsset available
-- with 66.666666 ADA available as reward,
t3 :: Assertion
t3 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC)])} =
            execState
              (spDeposits (zip [0 ..] [10_000, 10_000, 10_000]) >> liquidation >> liquidation >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt (OnChainDecimal 3333_333_333))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 66_666_666))
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt (OnChainDecimal 3333_333_333))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 66_666_666))
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 3333_333_333))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 66_666_666))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 333_333)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation :: State SPState ()
    liquidation = liquidate (10_000 * decimalUnit) (100 * decimalUnit)

-- | Depositors with equal initial deposit withdraw correct compounded
-- deposit and ADA Gain after three identical liquidations
-- 1. 3 Identical SP Deposits occur of 10k
-- 2. Three equal liquidations of a CDP with 10k iAsset and 100 ADA occurs.
-- 3. Each depositor expects to have 6666.666666 iAsset available
-- with 33.333333 ADA available as reward,
t4 :: Assertion
t4 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC)])} =
            execState
              (spDeposits (zip [0 ..] [10_000, 10_000, 10_000]) >> liquidation >> liquidation >> liquidation >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt (OnChainDecimal 0))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 100_000_000))
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt (OnChainDecimal 0))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 100_000_000))
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 0))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 100_000_000))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 1_000_000)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 1)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation :: State SPState ()
    liquidation = liquidate (10_000 * decimalUnit) (100 * decimalUnit)

-- | Depositors with equal initial deposit withdraw correct compounded
-- deposit and ADA Gain after two liquidations of increasing iAsset
-- 1. 3 Identical SP Deposits occur of 10k
-- 2. Liquidation of a CDP with 5k iAsset and 500 ADA occurs.
-- 3. Liquidation of a CDP with 7k iAsset and 700 ADA occurs.
-- 4. Each depositor expects to have 6000 iAsset available
-- with 40 ADA available as reward,
t5 :: Assertion
t5 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC)])} =
            execState
              (spDeposits (zip [0 ..] [10_000, 10_000, 10_000]) >> liquidation1 >> liquidation2 >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt (OnChainDecimal 6000_000_000))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 40_000_000))
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt (OnChainDecimal 6000_000_000))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 40_000_000))
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 6000_000_000))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 40_000_000))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 600_000)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation1 :: State SPState ()
    liquidation1 = liquidate (5_000 * decimalUnit) (50 * decimalUnit)

    liquidation2 :: State SPState ()
    liquidation2 = liquidate (7_000 * decimalUnit) (70 * decimalUnit)

-- | Depositors with equal initial deposit withdraw correct compounded
-- deposit and ADA Gain after three liquidations of increasing iAsset
-- 1. 3 Identical SP Deposits occur of 10k
-- 2. Liquidation of a CDP with 5k iAsset and 500 ADA occurs.
-- 3. Liquidation of a CDP with 6k iAsset and 600 ADA occurs.
-- 4. Liquidation of a CDP with 7k iAsset and 700 ADA occurs.
-- 5. Each depositor expects to have 4000 iAsset available
-- with 60 ADA available as reward,
t6 :: Assertion
t6 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC)])} =
            execState
              (spDeposits (zip [0 ..] [10_000, 10_000, 10_000]) >> liquidation1 >> liquidation2 >> liquidation3 >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt (OnChainDecimal 4000_000_000))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 60_000_000))
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt (OnChainDecimal 4000_000_000))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 60_000_000))
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 4000_000_000))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 60_000_000))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 400_000)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation1 :: State SPState ()
    liquidation1 = liquidate (5_000 * decimalUnit) (50 * decimalUnit)

    liquidation2 :: State SPState ()
    liquidation2 = liquidate (6_000 * decimalUnit) (60 * decimalUnit)

    liquidation3 :: State SPState ()
    liquidation3 = liquidate (7_000 * decimalUnit) (70 * decimalUnit)

-- | Depositors with varying deposits withdraw correct compounded deposit
-- and ADA Gain after two identical liquidations
-- 1. SP Deposits occur of 10k, 20k, and 30k
-- 2. Two liquidations with 10k iAsset and 100 ADA occurs.
-- 4. Depositor 1 = [6666.666666 deposit, 33.333333 ADA],
-- Depositor 2 = [13333.333333 deposit, 66.666666 ADA],
-- Depositor 3 = [20000 deposit, 100 ADA]
t7 :: Assertion
t7 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC)])} =
            execState
              (spDeposits (zip [0 ..] [10_000, 20_000, 30_000]) >> liquidation >> liquidation >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt (OnChainDecimal 6666_666_666))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 33_333_333))
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt (OnChainDecimal 13333_333_333))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 66_666_666))
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 20000_000_000))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 100_000_000))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 666_666)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation :: State SPState ()
    liquidation = liquidate (10_000 * decimalUnit) (100 * decimalUnit)

-- | Depositors with varying deposits withdraw correct compounded deposit
-- and ADA Gain after three identical liquidations
-- 1. SP Deposits occur of 10k, 20k, and 30k
-- 2. Three equal liquidations with 10k iAsset and 100 ADA occurs.
-- 3. Depositor 1 = [5000 deposit, 50 ADA],
-- Depositor 2 = [10000 deposit, 100 ADA],
-- Depositor 3 = [20000 deposit, 150 ADA]
t8 :: Assertion
t8 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC)])} =
            execState
              (spDeposits (zip [0 ..] [10_000, 20_000, 30_000]) >> liquidation >> liquidation >> liquidation >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt (OnChainDecimal 5000_000_000))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 50_000_000))
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt (OnChainDecimal 10000_000_000))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 100_000_000))
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 15000_000_000))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 150_000_000))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 500_000)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation :: State SPState ()
    liquidation = liquidate (10_000 * decimalUnit) (100 * decimalUnit)

-- | Depositors with varying deposits withdraw correct compounded deposit
-- and ADA Gain after three varying liquidations
-- 1. 3 SP Deposits occur of 2k, 456k, and 13.1k
-- 2. Three liquidations: [207000 iAsset, 2160 ADA],
-- [5000 iAsset, 50 ADA], [46700 iAsset, 500 ADA]
-- 3. Depositor 1 = [5000 deposit, 50 ADA],
-- Depositor 2 = [10000 deposit, 100 ADA],
-- Depositor 3 = [20000 deposit, 150 ADA]
t9 :: Assertion
t9 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC)])} =
            execState
              (spDeposits (zip [0 ..] [2_000, 456_000, 13_100]) >> liquidation1 >> liquidation2 >> liquidation3 >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt (OnChainDecimal 901_719_380))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 11_504_988))
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt (OnChainDecimal 205_592_018_679))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 2_623_137_332))
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 5_906_261_940))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 75_357_673))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 450_859)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation1 :: State SPState ()
    liquidation1 = liquidate (207_000 * decimalUnit) (2_160 * decimalUnit)

    liquidation2 :: State SPState ()
    liquidation2 = liquidate (5_000 * decimalUnit) (50 * decimalUnit)

    liquidation3 :: State SPState ()
    liquidation3 = liquidate (46_700 * decimalUnit) (500 * decimalUnit)

-- | A, B, C Deposit -> 2 liquidations -> D deposits -> 1 liquidation.
-- All deposits and liquidations = 100 iAsset.
-- A, B, C, D withdraw correct iAsset deposit and ADA Gain
-- 1. 3 SP Deposits occur of 10k
-- 2. Two liquidations of 10k iAsset and 100 ADA
-- 3. Additional deposit of 10k iAsset
-- 4. Liquidation of 10k iAsset and 100 ADA
-- 3. Depositor 1 = [5000 deposit, 50 ADA],
-- Depositor 2 = [10000 deposit, 100 ADA],
-- Depositor 3 = [20000 deposit, 150 ADA]
t10 :: Assertion
t10 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC), (accSnapD, accRewardD)]} =
            execState
              (spDeposits (zip [0 ..] [10_000, 10_000, 10_000, 0]) >> liquidation >> liquidation >> spDeposits (zip [0 ..] [0, 0, 0, 10_000]) >> liquidation >> allClaimRewards)
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt (OnChainDecimal 1666_666_666))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 83_333_332))
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt (OnChainDecimal 1666_666_666))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 83_333_332))
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 1666_666_666))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 83_333_332))
            >> (fromSPInteger (snapshotD accSnapD) `compareWithEps` getOnChainInt (OnChainDecimal 5000_000_000))
            >> (accRewardD `compareWithEps` getOnChainInt (OnChainDecimal 49_999_999))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 500_000)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation :: State SPState ()
    liquidation = liquidate (10_000 * decimalUnit) (100 * decimalUnit)

-- | Pool-emptying liquidation increases epoch by one, resets scaleFactor
-- to 0, and resets P to 1e18
t11 :: Assertion
t11 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState =
            execState
              (spDeposits (zip [0 ..] [10_000, 10_000, 0, 0]) >> liquidation >> liquidation >> spDeposits (zip [0 ..] [0, 0, 10_000, 10_000]) >> liquidation >> liquidation >> allClaimRewards)
              initState
       in (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 1_000_000)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 2)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation :: State SPState ()
    liquidation = liquidate (10_000 * decimalUnit) (100 * decimalUnit)

-- | Deposit spans one scale factor change: Single depositor withdraws correct compounded deposit and ADA Gain after one liquidation

-- 1. A deposits 10_000
-- 2. L1 brings P close to boundary, i.e. 9e-9:
-- liquidate 9999.99991 iAsset and 100 ADA
-- 3. A withdraws all
-- 4. B deposits 10_000
-- 5. L2 of 9900 iAsset and 60 ADA, should bring P slightly
-- past boundary i.e. 1e-9 -> 1e-10
t12 :: Assertion
t12 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB)]} =
            execState
              ( spDeposits (zip [0 ..] [10_000, 0])
                  >> liquidation1
                  >> close 0
                  >> spDeposits (zip [0 ..] [0, 10_000])
                  >> liquidation2
                  >> allClaimRewards
              )
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` 0)
            >> (accRewardA `compareWithEps` getOnChainInt 100)
            >> ( fromSPInteger (snapshotD accSnapB)
                   `compareWithEps` getOnChainInt 100
               )
            >> (accRewardB `compareWithEps` getOnChainInt 60)
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 90_000)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 1)

    liquidation1 :: State SPState ()
    liquidation1 = liquidate 9999_999_910 (100 * decimalUnit)

    liquidation2 :: State SPState ()
    liquidation2 = liquidate (9_900 * decimalUnit) (60 * decimalUnit)

-- | Several deposits of varying amounts span one scale factor change. Depositors withdraws correct compounded deposit and ADA Gain after one liquidation
-- 1. A make deposit 10_000 iAsset
-- 2. L1 brings P to 1e-5*P. L1:  9999.9 iAsset and 100 ADA
-- 3. A withdraws all
-- 4. B,C D make deposit 10_000, 20_000, 30_000
-- 5. L2 decreases P again by 1e-5, over boundary. L2: 59_999.4
-- (near to the 60_000 iAsset total deposits)
t13 :: Assertion
t13 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = map snd . Map.toList -> [(_, _), (accSnapB, accRewardB), (accSnapC, accRewardC), (accSnapD, accRewardD)]} =
            execState
              ( spDeposits (zip [0 ..] [10_000, 0, 0, 0])
                  >> liquidation1
                  >> close 0
                  >> spDeposits (zip [0 ..] [0, 10_000, 20_000, 30_000])
                  >> liquidation2
                  >> allClaimRewards
              )
              initState
       in (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt (OnChainDecimal 100_000))
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 200_000))
            >> (fromSPInteger (snapshotD accSnapD) `compareWithEps` getOnChainInt (OnChainDecimal 300_000))
            >> (accRewardB `compareWithEps` getOnChainInt 100)
            >> (accRewardC `compareWithEps` getOnChainInt 200)
            >> (accRewardD `compareWithEps` getOnChainInt 300)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 1)

    liquidation1 :: State SPState ()
    liquidation1 = liquidate 9999_900_000 (100 * decimalUnit)

    liquidation2 :: State SPState ()
    liquidation2 = liquidate 59999_400_000 (600 * decimalUnit)

-- | Several deposits of 10000 iAsset span one scale factor change. Depositors withdraws correct compounded deposit and ADA Gain after one liquidation
-- 1. A make deposit 10_000
-- 2. L1 brings P to 0.0001P. L1:  9999.9 iAsset, 100 ADA
-- 3. B makes deposit 9999.9, brings SP to 10k
-- 4. L2 decreases P by(~1e-5)P. L2:  9999.9 iAsset, 100 ADA
-- 5. C makes deposit 9999.9, brings SP to 10k
-- 6. L3 decreases P by(~1e-5)P. L3:  9999.9 iAsset, 100 ADA
-- 7. D makes deposit 9999.9, brings SP to 10k
-- 8. L4 decreases P by(~1e-5)P. L4:  9999.9 iAsset, 100 ADA
t14 :: Assertion
t14 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC), (accSnapD, accRewardD)]} =
            execState
              ( spDeposits (zip [0 ..] [10_000, 0, 0, 0])
                  >> liquidation
                  >> spDeposits (zip [0 ..] [0, depositAmt, 0, 0])
                  >> liquidation
                  >> spDeposits (zip [0 ..] [0, 0, depositAmt, 0])
                  >> liquidation
                  >> spDeposits (zip [0 ..] [0, 0, 0, depositAmt])
                  >> liquidation
                  >> allClaimRewards
              )
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` 0)
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` 0)
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` 0)
            >> (fromSPInteger (snapshotD accSnapD) `compareWithEps` getOnChainInt (OnChainDecimal 99_999))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 100_000_999))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 99_999_999))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 99_999_999))
            >> (accRewardD `compareWithEps` getOnChainInt (OnChainDecimal 99_998_999))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 10_000)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 2)

    -- 9999.9 iassets
    depositAmt :: OnChainDecimal
    depositAmt = OnChainDecimal 9999_900_000

    liquidation :: State SPState ()
    liquidation = liquidate (getOnChainInt depositAmt) (100 * decimalUnit)

-- | Depositors withdraw correct compounded deposit after liquidation
-- empties the pool
-- 1. A, B deposit 10000
-- 2. L1 cancels 20000, 200
-- 3. C, D, E deposit 10000, 20000, 30000
-- 4. L2 cancels 10000, 100
t15 :: Assertion
t15 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB), (accSnapC, accRewardC), (accSnapD, accRewardD), (accSnapE, accRewardE)]} =
            execState
              ( spDeposits (zip [0 ..] [10_000, 10_000, 0, 0, 0])
                  >> liquidation1
                  >> spDeposits (zip [0 ..] [0, 0, 10_000, 20_000, 30_000])
                  >> liquidation2
                  >> allClaimRewards
              )
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` 0)
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` 0)
            >> (fromSPInteger (snapshotD accSnapC) `compareWithEps` getOnChainInt (OnChainDecimal 8_333_333_333))
            >> (fromSPInteger (snapshotD accSnapD) `compareWithEps` getOnChainInt (OnChainDecimal 16_666_666_666))
            >> (fromSPInteger (snapshotD accSnapE) `compareWithEps` getOnChainInt (OnChainDecimal 25_000_000_000))
            >> (accRewardA `compareWithEps` getOnChainInt (OnChainDecimal 100_000_000))
            >> (accRewardB `compareWithEps` getOnChainInt (OnChainDecimal 100_000_000))
            >> (accRewardC `compareWithEps` getOnChainInt (OnChainDecimal 16_666_666))
            >> (accRewardD `compareWithEps` getOnChainInt (OnChainDecimal 33_333_333))
            >> (accRewardE `compareWithEps` getOnChainInt (OnChainDecimal 49_999_999))
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 833_333)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 1)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation1 :: State SPState ()
    liquidation1 = liquidate (20_000 * decimalUnit) (200 * decimalUnit)

    liquidation2 :: State SPState ()
    liquidation2 = liquidate (10_000 * decimalUnit) (100 * decimalUnit)

-- | Small liquidated coll/debt, large deposits and ADA price
t16 :: Assertion
t16 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = map snd . Map.toList -> [(accSnapA, accRewardA), (accSnapB, accRewardB)]} =
            execState
              ( spDeposits (zip [0 ..] [product (P.replicate 20 10), product (P.replicate 20 10)])
                  >> liquidation
                  >> allClaimRewards
              )
              initState
       in (fromSPInteger (snapshotD accSnapA) `compareWithEps` getOnChainInt 99_999_999_999_999_997_500)
            >> (fromSPInteger (snapshotD accSnapB) `compareWithEps` getOnChainInt 99_999_999_999_999_997_500)
            >> (accRewardA `compareWithEps` 0)
            >> (accRewardB `compareWithEps` 0)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)

    liquidation :: State SPState ()
    liquidation = liquidate (5_000 * decimalUnit) 5

t17 :: Assertion
t17 = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = map snd . Map.toList -> [(_, accRewardA), (_, accRewardB), (_, accRewardC), (_, accRewardD), (_, accRewardE)]} =
            execState
              ( spDeposits (zip [0 ..] [10_000, 0, 0, 0, 0])
                  >> liquidation
                  >> spDeposits (zip [0 ..] [0, amt, 0, 0, 0])
                  >> liquidation
                  >> spDeposits (zip [0 ..] [0, 0, amt, 0, 0])
                  >> liquidation
                  >> spDeposits (zip [0 ..] [0, 0, 0, amt, 0])
                  >> liquidation
                  >> spDeposits (zip [0 ..] [0, 0, 0, 0, amt])
                  >> liquidation
                  >> allClaimRewards
              )
              initState
       in (accRewardA `compareWithEps` 1_000_010_000_100)
            >> (accRewardB `compareWithEps` 1_000_000_000_000)
            >> (accRewardC `compareWithEps` 1_000_000_000_000)
            >> (accRewardD `compareWithEps` 999_999_999_900)
            >> (accRewardE `compareWithEps` 999_990_000_000)
            >> (fromSPIntegerToOCD (snapshotP (sPoolSnapshot spState)) `compareWithEps` OnChainDecimal 100_000_000_000)
            >> (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 2)

    amt :: OnChainDecimal
    amt = OnChainDecimal 9999_900_000

    liquidation :: State SPState ()
    liquidation = liquidate (getOnChainInt amt) (1_000_000 * decimalUnit)

-- | Check that when deposit decreases below 1e9th of the initial deposit, deposit becomes 0.
t18 :: Assertion
t18 = case1 >> case2
  where
    case1 :: Assertion
    case1 =
      let SPState
            { accountsSnapshotsWithRewards =
                map snd . Map.toList -> [(accSnapA, _)]
            } =
              execState
                ( spDeposits (zip [0 ..] [10_000])
                    >> liquidation1
                    >> allClaimRewards
                )
                initState
       in (fromSPInteger (snapshotD accSnapA) @?= 0)

    case2 :: Assertion
    case2 =
      let SPState
            { accountsSnapshotsWithRewards =
                map snd . Map.toList -> [(accSnapA, _)]
            } =
              execState
                ( spDeposits (zip [0 ..] [10_000])
                    >> liquidation2
                    >> allClaimRewards
                )
                initState
       in (fromSPInteger (snapshotD accSnapA) @?= 1_000)

    liquidation1 :: State SPState ()
    liquidation1 = liquidate 9999_999_999 (100 * decimalUnit)

    liquidation2 :: State SPState ()
    liquidation2 = liquidate 9999_999_000 (100 * decimalUnit)

-- | Test case with rounding issue
-- 1. U1 Opens Stability Pool Account - 6_250_000
-- 2. U2 Opens Stability Pool Account - 12_500_000
-- 3. Liquidate 6_250_000
-- 4. U2 Closes
tNewCase :: Assertion
tNewCase = afterLiquidation
  where
    afterLiquidation :: Assertion
    afterLiquidation =
      let spState@SPState {accountsSnapshotsWithRewards = (map snd . Map.toList -> [(_, _), (_, _)])} =
            execState
              (spDeposits (zip [0 ..] [625, 1250]) >> l1 >> close 1 >> close 0)
              initState
       in (snapshotEpoch (sPoolSnapshot spState) @?= 0)
            >> (snapshotScale (sPoolSnapshot spState) @?= 0)
            >> (snapshotD (sPoolSnapshot spState) @?= toSPInteger 0)

    l1 :: State SPState ()
    l1 = liquidate (625 * decimalUnit) (100 * decimalUnit)
