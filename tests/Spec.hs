module Main (main) where

import Options (SuiteOptions (All, Benchmark, Test))
import Options.Applicative
  ( Alternative ((<|>)),
    ParserInfo,
    execParserPure,
    flag',
    fullDesc,
    handleParseResult,
    help,
    helper,
    info,
    long,
    prefs,
    progDesc,
    showHelpOnError,
  )
import Plutus.Model (defaultBabbage)
import PlutusTx.Prelude
import Spec.AdaptiveQuorumSpec qualified as AQB
import Spec.CDP.Benchmark qualified as CDP
import Spec.Collector.Benchmark qualified as Collector
import Spec.Governance.Benchmark qualified as Gov
import Spec.Liquidity.Benchmark qualified as Liquidity
import Spec.Oracle.Benchmark qualified as Oracle
import Spec.StabilityPool.Benchmark qualified as SP
import Spec.StabilityPool.Test qualified as SPSnap
import Spec.Staking.Benchmark qualified as Staking
import System.Environment (getArgs, withArgs)
import Test.Tasty (testGroup)
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.Basic (consoleTestReporter)
import Test.Tasty.Runners (defaultMainWithIngredients)
import Test.Tasty.Runners.AntXML
import Prelude (IO)
import Prelude qualified as P

suiteParserInfo :: ParserInfo SuiteOptions
suiteParserInfo =
  info
    ( helper
        P.<*> ( flag'
                  All
                  (long "all-suites" P.<> help "Run both tests and benchmarks")
                  <|> flag' Test (long "test" P.<> help "Run only tests")
                  <|> flag'
                    Benchmark
                    (long "benchmark" P.<> help "Run only benchmarks")
              )
    )
    ( fullDesc
        P.<> progDesc
          "Select the suite to run and optionally \
          \add tasty arguments after the \"--\"."
    )

main :: IO ()
main = do
  allArgs <- getArgs
  let customArgs = P.takeWhile (P./= "--") allArgs
      tastyArgs = P.drop (1 P.+ P.length customArgs) allArgs
  suiteOpts <- case allArgs of
    [] -> P.pure Test
    _ ->
      handleParseResult
        (execParserPure (prefs showHelpOnError) suiteParserInfo customArgs)

  withArgs
    tastyArgs
    ( do
        let cfg = defaultBabbage
        aqbTests <- AQB.tests suiteOpts
        defaultMainWithIngredients
          [antXMLRunner `composeReporters` consoleTestReporter]
          $ testGroup
            "benchmark test suite"
            [ CDP.tests suiteOpts cfg,
              Collector.tests suiteOpts cfg,
              Oracle.tests suiteOpts cfg,
              SP.tests suiteOpts cfg,
              Gov.tests suiteOpts cfg,
              Staking.tests suiteOpts cfg,
              Liquidity.tests suiteOpts cfg,
              aqbTests,
              SPSnap.tests suiteOpts
            ]
    )
