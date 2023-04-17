module Options
  ( SuiteOptions (All, Test, Benchmark),
    setupTestBenchmarkSuite,
  )
where

import Test.Tasty (TestTree)
import Prelude qualified as P

data SuiteOptions = All | Test | Benchmark deriving (P.Show)

setupTestBenchmarkSuite ::
  SuiteOptions -> [TestTree] -> [TestTree] -> [TestTree]
setupTestBenchmarkSuite suiteOpts tests benchmarks =
  case suiteOpts of
    All -> tests P.++ benchmarks
    Test -> tests
    Benchmark -> benchmarks
