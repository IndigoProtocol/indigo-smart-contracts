{-# LANGUAGE NamedFieldPuns #-}

module Spec.AdaptiveQuorumSpec (tests) where

import Data.ByteString.Lazy qualified as BL
import Data.Char (isDigit)
import Data.Csv (HasHeader (HasHeader), decode)
import Data.List.Split (splitOn)
import Data.Time (Day, diffDays, fromGregorian)
import Data.Vector (Vector, toList)
import Indigo.Contracts.Governance.Poll.Common
  ( DistributionSchedule
      ( z_ipd,
        z_lpd,
        z_spd,
        z_tv,
        λM_ipd,
        λM_lpd,
        λM_spd,
        λM_tv
      ),
    PollStatus (VoteCount, nNo, nYes),
    electorate,
    pollPassQuorum,
    q,
    ve,
    vm,
  )
import Indigo.Data.Decimal (OnChainDecimal (OnChainDecimal))
import Indigo.Data.Decimal qualified as OCD
import Indigo.Utils.Helpers (oneDay)
import Ledger (POSIXTime (POSIXTime), getPOSIXTime)
import Options (SuiteOptions, setupTestBenchmarkSuite)
import PlutusTx.Prelude hiding (error, toList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@=?), (@?), (@?=))
import Utils.Mock qualified as Mock
import Prelude (Double, IO, Int, Real, Show, String)
import Prelude qualified as P

type SeedItem =
  (Integer, Integer, String, (Integer, Int, Int), Integer, OnChainDecimal, Bool)

data Config = Config
  { launch :: Day,
    distSchedule :: DistributionSchedule
  }

tests :: SuiteOptions -> IO TestTree
tests suiteOpts = do
  csvData <- BL.readFile "tests/data/AQB.csv"
  let seed = parseSeed csvData
      seedConf =
        Config
          { launch = fromGregorian 2022 10 12,
            distSchedule = Mock.distributionSchedule {z_tv = 19}
          }
      specs =
        [ veVmTests
            ( Config
                { launch = fromGregorian 2022 8 18,
                  distSchedule = Mock.distributionSchedule
                }
            ),
          qTests seedConf seed,
          electorateTests seedConf seed,
          pollPassQuorumTests seedConf seed
        ]
   in P.pure $
        testGroup
          "Adaptive Quorum Biasing"
          (setupTestBenchmarkSuite suiteOpts specs [])

type Decoded =
  Either String (Vector (String, String, String, String, String, String))

parseSeed :: BL.ByteString -> [SeedItem]
parseSeed csvData =
  case decode HasHeader csvData :: Decoded of
    Left s -> P.error s
    Right vec -> toList $ P.fmap parse vec
  where
    parse :: (String, String, String, String, String, String) -> SeedItem
    parse (yes, no, date, e, q', pass) =
      let [year, month, day] = splitOn "-" date
       in ( P.read @Integer (filter isDigit yes),
            P.read @Integer (filter isDigit no),
            date,
            (P.read @Integer year, P.read @Int month, P.read @Int day),
            P.read @Integer (filter isDigit e),
            OnChainDecimal
              (P.round $ P.read @Double q' P.* P.fromInteger OCD.decimalUnit),
            case pass of
              "Yes" -> True
              "No" -> False
              _ -> P.error "Could not parse pass"
          )

pollPassQuorumTests :: Config -> [SeedItem] -> TestTree
pollPassQuorumTests Config {launch, distSchedule} seed =
  let endTime' = endTime launch
   in testGroup
        "Poll pass quorum"
        $ testCase
          "No votes does not pass"
          ( False
              @=? pollPassQuorum
                (VoteCount {nYes = 0, nNo = 0})
                0
                Mock.initialIndyDistribution
                Mock.totalINDYSupply
                (endTime' (fromGregorian 2022 10 24))
                distSchedule
          )
          : fmap
            ( \(nYes, nNo, date, (year, month, day), _, _, pass) ->
                testCase date $
                  pass
                    @=? pollPassQuorum
                      (VoteCount {nYes, nNo})
                      0
                      Mock.initialIndyDistribution
                      Mock.totalINDYSupply
                      (endTime' (fromGregorian year month day))
                      distSchedule
            )
            seed

qTests :: Config -> [SeedItem] -> TestTree
qTests Config {launch, distSchedule} seed =
  let acceptableError :: Double
      acceptableError = 0.00001
      assertWithAcceptableError' =
        assertWithAroundAcceptableError acceptableError
      endTime' = endTime launch
   in testGroup
        "Q calculation"
        $ testCase
          "No votes results in q=0"
          ( q
              (VoteCount {nYes = 0, nNo = 0})
              0
              Mock.initialIndyDistribution
              Mock.totalINDYSupply
              (endTime' (fromGregorian 2022 10 24))
              distSchedule
              @?= 0
          )
          : fmap
            ( \(nYes, nNo, date, (year, month, day), _, q', _) ->
                if abs q' < 10
                  then
                    testCase date $
                      assertFitsInThreshold
                        0.001
                        q'
                        ( q
                            (VoteCount {nYes, nNo})
                            0
                            Mock.initialIndyDistribution
                            Mock.totalINDYSupply
                            (endTime' (fromGregorian year month day))
                            distSchedule
                        )
                  else
                    testCase date $
                      assertWithAcceptableError'
                        q'
                        ( q
                            (VoteCount {nYes, nNo})
                            0
                            Mock.initialIndyDistribution
                            Mock.totalINDYSupply
                            (endTime' (fromGregorian year month day))
                            distSchedule
                        )
            )
            seed

electorateTests :: Config -> [SeedItem] -> TestTree
electorateTests Config {launch, distSchedule} seed =
  let acceptableError :: Double
      acceptableError = 0.000001
      assertWithAcceptableError' =
        assertWithLowerBoundAcceptableError acceptableError
      endTime' = endTime launch
   in testGroup
        "Electorate calculation"
        $ fmap
          ( \(_, _, date, (year, month, day), e, _, _) ->
              testCase date $
                assertWithAcceptableError'
                  e
                  ( electorate
                      Mock.initialIndyDistribution
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian year month day))
                      distSchedule
                  )
          )
          seed

-- |  Mirrors:
-- https://github.com/IndigoProtocol/indy-tokenomics/blob/main/tests/test_calcs.py
-- Additionally, with acceptable error because of the use of fixed
-- decimal places in calculations.
veVmTests :: Config -> TestTree
veVmTests Config {launch, distSchedule} =
  let acceptableError :: Double
      acceptableError = 0.000001
      assertWithAcceptableError' =
        assertWithLowerBoundAcceptableError acceptableError
      endTime' = endTime launch
   in testGroup
        "v_e and v_m calculation"
        [ testGroup
            "v_e ipd test cases"
            [ testCase "2021-08-20 v_e ipd" $
                assertWithAcceptableError'
                  0
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2021 8 20))
                      (z_ipd distSchedule)
                      (λM_ipd distSchedule)
                  ),
              testCase "2022-08-20 v_e ipd" $
                assertWithAcceptableError'
                  2398
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2022 8 20))
                      (z_ipd distSchedule)
                      (λM_ipd distSchedule)
                  ),
              testCase "2024-09-23 v_e ipd" $
                assertWithAcceptableError'
                  475922
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2024 9 23))
                      (z_ipd distSchedule)
                      (λM_ipd distSchedule)
                  ),
              testCase "2027-08-27 v_e ipd" $
                assertWithAcceptableError'
                  1750000
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2027 8 27))
                      (z_ipd distSchedule)
                      (λM_ipd distSchedule)
                  ),
              testCase "2030-01-01 v_e ipd" $
                assertWithAcceptableError'
                  1750000
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2030 1 1))
                      (z_ipd distSchedule)
                      (λM_ipd distSchedule)
                  )
            ],
          testGroup
            "v_e lpd test cases"
            [ testCase "2021-08-20 v_e lpd" $
                assertWithAcceptableError'
                  0
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2021 8 20))
                      (z_lpd distSchedule)
                      (λM_lpd distSchedule)
                  ),
              testCase "2022-08-20 v_e lpd" $
                assertWithAcceptableError'
                  0
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2022 8 20))
                      (z_lpd distSchedule)
                      (λM_lpd distSchedule)
                  ),
              testCase "2022-11-16 v_e lpd" $
                assertWithAcceptableError'
                  4795
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2022 11 16))
                      (z_lpd distSchedule)
                      (λM_lpd distSchedule)
                  ),
              testCase "2023-11-17 v_e lpd" $
                assertWithAcceptableError'
                  359625
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2023 11 17))
                      (z_lpd distSchedule)
                      (λM_lpd distSchedule)
                  ),
              testCase "2024-10-26 v_e lpd" $
                assertWithAcceptableError'
                  1021324
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2024 10 26))
                      (z_lpd distSchedule)
                      (λM_lpd distSchedule)
                  ),
              testCase "2027-11-27 v_e lpd" $
                assertWithAcceptableError'
                  5250000
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2027 11 27))
                      (z_lpd distSchedule)
                      (λM_lpd distSchedule)
                  ),
              testCase "2030-01-01 v_e lpd" $
                assertWithAcceptableError'
                  5250000
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2030 1 1))
                      (z_lpd distSchedule)
                      (λM_lpd distSchedule)
                  )
            ],
          testGroup
            "v_e spd test cases"
            [ testCase "2021-08-20 v_e spd" $
                assertWithAcceptableError'
                  0
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2021 8 20))
                      (z_spd distSchedule)
                      (λM_spd distSchedule)
                  ),
              testCase "2022-08-20 v_e spd" $
                assertWithAcceptableError'
                  0
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2022 8 20))
                      (z_spd distSchedule)
                      (λM_spd distSchedule)
                  ),
              testCase "2022-09-17 v_e spd" $
                assertWithAcceptableError'
                  28768
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2022 9 17))
                      (z_spd distSchedule)
                      (λM_spd distSchedule)
                  ),
              testCase "2022-12-25 v_e spd" $
                assertWithAcceptableError'
                  575360
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2022 12 25))
                      (z_spd distSchedule)
                      (λM_spd distSchedule)
                  ),
              testCase "2027-05-20 v_e spd" $
                assertWithAcceptableError'
                  12897265
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2027 5 20))
                      (z_spd distSchedule)
                      (λM_spd distSchedule)
                  ),
              testCase "2027-09-11 v_e spd" $
                assertWithAcceptableError'
                  14000000
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2027 9 11))
                      (z_spd distSchedule)
                      (λM_spd distSchedule)
                  ),
              testCase "2030-01-01 v_e spd" $
                assertWithAcceptableError'
                  14000000
                  ( ve
                      Mock.totalINDYSupply
                      0
                      (endTime' (fromGregorian 2030 1 1))
                      (z_spd distSchedule)
                      (λM_spd distSchedule)
                  )
            ],
          testGroup
            "v_m tv (delay = 13) test cases"
            ( let delay = 13
               in [ testCase "2021-08-17 v_m tv" $
                      assertWithAcceptableError'
                        0
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2021 8 17))
                            delay
                            (λM_tv distSchedule)
                        ),
                    testCase "2022-08-17 v_m tv" $
                      assertWithAcceptableError'
                        0
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2022 8 17))
                            delay
                            (λM_tv distSchedule)
                        ),
                    testCase "2022-08-18 v_m tv" $
                      assertWithAcceptableError'
                        328125
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2022 8 18))
                            delay
                            (λM_tv distSchedule)
                        ),
                    testCase "2022-08-25 v_m tv" $
                      assertWithAcceptableError'
                        328125
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2022 8 25))
                            delay
                            (λM_tv distSchedule)
                        ),
                    testCase "2022-08-31 v_m tv" $
                      assertWithAcceptableError'
                        328125
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2022 8 31))
                            delay
                            (λM_tv distSchedule)
                        ),
                    testCase "2023-03-25 v_m tv" $
                      assertWithAcceptableError'
                        2296875
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2023 3 25))
                            delay
                            (λM_tv distSchedule)
                        ),
                    testCase "2023-04-01 v_m tv" $
                      assertWithAcceptableError'
                        2625000
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2023 4 1))
                            delay
                            (λM_tv distSchedule)
                        ),
                    testCase "2023-12-02 v_m tv" $
                      assertWithAcceptableError'
                        5250000
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2023 12 2))
                            delay
                            (λM_tv distSchedule)
                        ),
                    testCase "2024-07-18 v_m tv" $
                      assertWithAcceptableError'
                        7546875
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2024 7 18))
                            delay
                            (λM_tv distSchedule)
                        ),
                    testCase "2024-07-31 v_m tv" $
                      assertWithAcceptableError'
                        7875000
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2024 7 31))
                            delay
                            (λM_tv distSchedule)
                        ),
                    testCase "2030-01-01 v_m tv" $
                      assertWithAcceptableError'
                        7875000
                        ( vm
                            Mock.totalINDYSupply
                            0
                            (endTime' (fromGregorian 2030 1 1))
                            delay
                            (λM_tv distSchedule)
                        )
                  ]
            )
        ]

-- |  Check if the value fits into range where only the
-- lower bound is moved by the error.
assertWithLowerBoundAcceptableError ::
  (Real a, Show a) => Double -> a -> a -> Assertion
assertWithLowerBoundAcceptableError err expected actual =
  let lower =
        P.realToFrac
          expected
          P.* (1.0 P.- P.realToFrac (P.signum expected) P.* err)
   in lower P.<= P.realToFrac actual
        && (actual P.<= expected)
        @? ( "The result is not in acceptable error range. Problem: "
               <> P.show lower
               <> " <= "
               <> P.show (P.realToFrac @_ @Double actual)
               <> " <= "
               <> P.show (P.realToFrac @_ @Double expected)
           )

-- | Check if the value fits into error range
assertWithAroundAcceptableError ::
  (Real a, Show a) => Double -> a -> a -> Assertion
assertWithAroundAcceptableError err expected actual =
  let dist = (P.realToFrac . P.abs) (expected P.- actual)
      maxErr = P.abs (P.realToFrac expected P.* err)
   in (dist P.<= maxErr)
        @? ( "The result is not in acceptable error range. Actual: "
               <> P.show actual
               <> " Expected: "
               <> P.show expected
               <> " Problem: "
               <> P.show dist
               <> " <= "
               <> P.show maxErr
           )

endTime :: Day -> Day -> Ledger.POSIXTime
endTime launch' d = POSIXTime $ getPOSIXTime oneDay * daysDiffFromStart d
  where
    daysDiffFromStart :: Day -> Integer
    daysDiffFromStart = flip diffDays launch'

assertFitsInThreshold :: (Real a, Show a) => Double -> a -> a -> Assertion
assertFitsInThreshold threshold expected actual =
  let dist = P.realToFrac (P.abs (expected P.- actual))
   in (threshold P.> dist)
        @? ( "Actual value is larger than acceptable threshold. Actual: "
               <> P.show actual
               <> " Expected: "
               <> P.show expected
               <> " Problem: "
               <> P.show threshold
               <> " > "
               <> P.show dist
           )
