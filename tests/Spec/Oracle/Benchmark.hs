module Spec.Oracle.Benchmark
  ( tests,
    initialFunds,
  )
where

import Control.Monad
import Indigo.Contracts.CDP.Common (iBTCTokenName, iETHTokenName)
import Indigo.Data.Decimal (OnChainDecimal (OnChainDecimal))
import Indigo.Utils.Helpers (unitValue)
import Ledger.Ada as Ada
import Ledger.Value
import Options (SuiteOptions, setupTestBenchmarkSuite)
import Plutus.Model
import PlutusTx.Prelude hiding (check)
import Spec.Oracle.Helpers (oAssetNFTAssetClass)
import Spec.Oracle.Transactions
  ( FeedPriceVariation (..),
    feedPrice,
    runInitRefScript,
    startOracle,
  )
import Test.Tasty
import Utils.Helpers
import Prelude (String)

tests :: SuiteOptions -> MockConfig -> TestTree
tests suiteOpts cfg =
  let specs =
        [ check
            "start oracle test"
            (void $ startOracle iBTCTokenName (OnChainDecimal 1_000_000)),
          check "feed oracle test" (feedOracle' iBTCTokenName FeedPriceSucceed),
          checkFailWithMsg'
            "feed price fails if expiration time is set to the past"
            (ErrorMsgContains "Expiration time is not properly set")
            (feedOracle' iBTCTokenName ExpirationInPast),
          checkFailWithMsg'
            "feed price fails if expiration time is set to far future"
            (ErrorMsgContains "Expiration time is not properly set")
            (feedOracle' iBTCTokenName ExpirationInFarFuture)
        ]
      benchmarks =
        [ assertLimits initialFunds cfg "init ref script" runInitRefScript,
          assertLimits
            initialFunds
            cfg
            "start oracle"
            (void $ startOracle iBTCTokenName (OnChainDecimal 1_000_000)),
          assertLimits
            initialFunds
            cfg
            "feed oracle"
            (feedOracle' iBTCTokenName FeedPriceSucceed)
        ]
   in testGroup "Oracle" (setupTestBenchmarkSuite suiteOpts specs benchmarks)
  where
    check :: String -> Run () -> TestTree
    check = checkNoFail cfg initialFunds

    checkFailWithMsg' :: String -> ErrorMsgContains -> Run () -> TestTree
    checkFailWithMsg' = checkFailWithMsg cfg initialFunds

    feedOracle' :: TokenName -> FeedPriceVariation -> Run ()
    feedOracle' token v = do
      void $ startOracle token (OnChainDecimal 100_000_000)
      feedPrice token (OnChainDecimal 200_000_000) v

initialFunds :: Value
initialFunds =
  adaValueOf 10_000_000
    <> unitValue (oAssetNFTAssetClass iBTCTokenName)
    <> unitValue (oAssetNFTAssetClass iETHTokenName)
