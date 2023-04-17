module Spec.Collector.Benchmark (tests) where

import Ledger qualified
import Ledger.Ada qualified as Ada
import Options (SuiteOptions, setupTestBenchmarkSuite)
import Plutus.Model
import Spec.Collector.Transactions
  ( runCollectorDoubleSatisfaction,
    runDirectlySpendFunds,
    runDirectlySpendFundsExploit,
    runInitCollector,
  )
import Test.Tasty
import Utils.Helpers
  ( ErrorMsgContains (ErrorMsgContains),
    checkFailWithMsg,
    checkNoFail,
  )
import Prelude

tests :: SuiteOptions -> MockConfig -> TestTree
tests suiteOpts cfg =
  let specs =
        [ check
            "Initialize Collector"
            (runInitCollector 1 Nothing),
          checkFailWithMsg'
            "Attempt to directly withdraw funds from Collector fails"
            (ErrorMsgContains "increase ADA value with just 1 input")
            runDirectlySpendFunds,
          checkFailWithMsg'
            "Attempt to directly withdraw funds from Collector fails with staking credential"
            (ErrorMsgContains "increase ADA value with just 1 input")
            runDirectlySpendFundsExploit,
          checkFailWithMsg'
            "Collector Double Satisfaction fails"
            (ErrorMsgContains "increase ADA value with just 1 input")
            runCollectorDoubleSatisfaction
        ]
      benchmarks = []
   in testGroup "Collector" (setupTestBenchmarkSuite suiteOpts specs benchmarks)
  where
    check :: String -> Run () -> TestTree
    check = checkNoFail cfg initialFunds

    checkFailWithMsg' :: String -> ErrorMsgContains -> Run () -> TestTree
    checkFailWithMsg' = checkFailWithMsg cfg initialFunds

initialFunds :: Ledger.Value
initialFunds = Ada.adaValueOf 10_000_000_000
