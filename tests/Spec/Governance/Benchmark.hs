module Spec.Governance.Benchmark (tests, initialFunds) where

import Control.Monad (void)
import Indigo.Contracts.CDP.Common (iBTCTokenName)
import Indigo.Contracts.CDP.Common qualified as CDPCommon
import Indigo.Contracts.Governance.Gov.Common qualified as Gov
import Indigo.Contracts.Governance.Gov.Common qualified as GovParams
import Indigo.Contracts.Governance.Poll.Common qualified as Poll
import Indigo.Contracts.Staking.Common qualified as StakingParams
import Indigo.Data.Decimal (OnChainDecimal (OnChainDecimal))
import Indigo.Utils.Helpers
import Ledger (POSIXTime, Value)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Options (SuiteOptions, setupTestBenchmarkSuite)
import Plutus.Model
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (check, (*))
import Spec.CDP.Params qualified as CDPParams
import Spec.CDP.Transactions qualified as CDP
import Spec.Collector.Script (collectorScript)
import Spec.Governance.Params
  ( CreateProposalParam (CreateProposalParam),
    CreateShardsParam (CreateShardsParam),
    EndParam (EndParam),
    MergeShardsParam (MergeShardsParam),
    VoteParam (VoteParam),
  )
import Spec.Governance.Script (govScript, upgradePolicyCurrency)
import Spec.Governance.Transactions
  ( CreateProposalResult,
    createAssetProposal,
    createDelistProposal,
    createMigrateProposal,
    executeProposalExploitSuccessfully,
    executeProposalSuccessfully,
    identityDatumUpgradeCompiled,
    runCollectorUpgrade,
    runCreateProposal,
    runCreateShards,
    runCreateShardsChunks,
    runEndProposal,
    runGovUpgrade,
    runInitRefScriptExecute,
    runInitRefScriptGov,
    runInitRefScriptPoll,
    runInitRefScriptPollManager,
    runInitialize,
    runMergeShards,
    runMergeShardsChunks,
    runMergeShardsExploit,
    runVote,
    versionIncrementGovDatumUpgrade,
    versionIncrementGovDatumUpgradeCompiled,
  )
import Spec.Oracle.Benchmark qualified as OracleBenchmark
import Spec.Oracle.Helpers (waitUntilOracleExpired)
import Spec.Oracle.Transactions (startOracle)
import Spec.StabilityPool.Params qualified as SPParams
import Spec.StabilityPool.Transactions qualified as SP
import Test.Tasty
import Utils.Helpers
import Utils.Mock qualified as Mock
import Prelude (Int, String, (*))

tests :: SuiteOptions -> MockConfig -> TestTree
tests suiteOpts cfg =
  let base =
        [ testGroup
            "Create Asset"
            ( proposalRoundtripTests
                ( runInitialize
                    >> startOracle iBTCTokenName (OnChainDecimal 1_000_000)
                    >>= \oracleAssetNft ->
                      createAssetProposal
                        iBTCTokenName
                        150
                        oracleAssetNft
                )
            ),
          testGroup
            "Upgrade Protocol"
            ( proposalRoundtripTests
                (runInitialize >> createUpgradeProposal AssocMap.empty)
            ),
          testGroup
            "Text Proposal"
            (proposalRoundtripTests (runInitialize >> createTextProposal)),
          testGroup
            "Delist Asset"
            ( proposalRoundtripTests
                ( runInitialize
                    >> startOracle iBTCTokenName (OnChainDecimal 1_000_000)
                    >>= createAssetProposal iBTCTokenName 150
                    >>= executeProposalSuccessfully
                    >> createDelistProposal iBTCTokenName
                )
            ),
          testGroup
            "Migrate Asset"
            ( proposalRoundtripTests
                ( do
                    assetNft <-
                      runInitialize
                        >> startOracle iBTCTokenName (OnChainDecimal 1_000_000)
                    createAssetProposal iBTCTokenName 150 assetNft
                      >>= executeProposalSuccessfully
                    createMigrateProposal
                      iBTCTokenName
                      ( OnChainDecimal
                          200_000_000
                      )
                      assetNft
                )
            ),
          testGroup
            "Modify Protocol params"
            ( proposalRoundtripTests
                (runInitialize >> createModifyProtocolParamsProposal)
            )
        ]
      specs =
        [ check "init gov succeeds" runInitialize,
          check
            "Usage of IAsset after creation \
            \(Gov Create IAsset -> CDP open -> SP create -> SP close)"
            ( do
                runInitialize
                oracleAssetNft <-
                  startOracle iBTCTokenName (OnChainDecimal 1_000_000)
                createAssetProposal iBTCTokenName 150 oracleAssetNft
                  >>= executeProposalSuccessfully

                user <- newUser mempty
                CDP.runOpenCDPPosition
                  user
                  CDPParams.OpenPositionParam
                    { CDPParams.opAsset = iBTCTokenName,
                      CDPParams.opCollateralAmount = 15_000_000,
                      CDPParams.opMintedAmount = 500
                    }
                  CDP.Succeed
                SP.runCreate
                  user
                  SPParams.CreateParam
                    { SPParams.cTokenName = iBTCTokenName,
                      SPParams.cAmount = 500
                    }
                  SP.OpenSucceed
                SP.runClose
                  user
                  SPParams.CloseParam {SPParams.clTokenName = iBTCTokenName}
            ),
          testGroup
            "Upgrades"
            [ check
                "Execute identity Gov datum Upgrade proposal\
                \ and upgrade correctly"
                (runInitialize >> identityGovUpgrade),
              check "collector upgrade doesn't work" (runInitialize >> identityCollectorUpgrade),
              check
                "Execute version increment Gov datum Upgrade proposal \
                \and upgrade correctly"
                (runInitialize >> versionIncrementGovUpgrade),
              check
                "Execute Gov parameter Upgrade proposal and upgrade correctly"
                (runInitialize >> paramsGovUpgrade)
            ],
          check
            "migrate asset procedure suceeds also when oracle expired"
            ( do
                assetNft <-
                  runInitialize
                    >> startOracle iBTCTokenName (OnChainDecimal 1_000_000)
                waitUntilOracleExpired
                createAssetProposal iBTCTokenName 150 assetNft
                  >>= executeProposalSuccessfully
                createMigrateProposal
                  iBTCTokenName
                  (OnChainDecimal 200_000_000)
                  assetNft
                  >>= executeProposalSuccessfully
            ),
          checkFailWithMsg'
            "migrate delisted asset fails"
            (ErrorMsgContains "Cannot migrate a delisted iAsset")
            ( do
                runInitialize
                oracleAssetNft <-
                  startOracle iBTCTokenName (OnChainDecimal 1_000_000)
                createAssetProposal iBTCTokenName 150 oracleAssetNft
                  >>= executeProposalSuccessfully
                createDelistProposal iBTCTokenName
                  >>= executeProposalSuccessfully
                createMigrateProposal
                  iBTCTokenName
                  (OnChainDecimal 200_000_000)
                  oracleAssetNft
                  >>= executeProposalSuccessfully
            ),
          checkFailWithMsg'
            "Can only consume numShardInputs + 1 poll manager"
            (ErrorMsgContains "Can only consume numShardInputs + 1 poll manager")
            (runInitialize >> createTextProposal >>= mergeShardsExploit),
          checkFailWithMsg'
            "Cannot upgrade two proposals at the same time"
            (ErrorMsgContains "Can only consume one upgrade token")
            ( do
                runInitialize

                -- Creating two text proposals
                r1 <- createTextProposal
                r2 <- createTextProposal

                executeProposalExploitSuccessfully r1 r2
            )
        ]
          ++ base
      benchmarks =
        testGroup
          "Upgrades"
          [ assertLimits
              initialFunds
              cfg
              "Execute identity Gov Upgrade proposal and upgrade correctly"
              (runInitialize >> identityGovUpgrade),
            assertLimits
              initialFunds
              cfg
              "Execute Gov parameter Upgrade proposal and upgrade correctly"
              (runInitialize >> paramsGovUpgrade),
            assertLimits
              initialFunds
              cfg
              "Execute version increment Gov datum upgrade proposal \
              \and upgrade correctly"
              (runInitialize >> paramsGovUpgrade),
            assertLimits
              initialFunds
              cfg
              "init ref script for gov"
              runInitRefScriptGov,
            assertLimits
              initialFunds
              cfg
              "init ref script for poll manager"
              runInitRefScriptPollManager,
            assertLimits
              initialFunds
              cfg
              "init ref script for poll"
              runInitRefScriptPoll,
            assertLimits
              initialFunds
              cfg
              "init ref script for execute"
              runInitRefScriptExecute
          ]
          : base
   in testGroup
        "Governance"
        (setupTestBenchmarkSuite suiteOpts specs benchmarks)
  where
    proposalRoundtripTests :: Run CreateProposalResult -> [TestTree]
    proposalRoundtripTests create =
      let testData =
            [ ("create proposal succeeds", void create),
              ("create all shards succeeds", create >>= createShards'),
              ( "create shards in chunks of 5 succeeds (stats for 5)",
                create >>= createShardsChunks' 5
              ),
              ( "create shards in chunks of 7 succeeds (stats for 3)",
                create >>= createShardsChunks' 7
              ),
              ("merge all shards no votes succeeds", create >>= mergeShards'),
              ("vote succeeds", create >>= vote'),
              ( "merge all shards with votes succeeds",
                create >>= mergeShardsWithVotes'
              ),
              ( "merge shards with votes in chunks of 6 succeeds (stats for 4)",
                create >>= mergeShardsWithVotesChunks' 6
              ),
              ( "merge shards with votes in chunks of 7 succeeds (stats for 3)",
                create >>= mergeShardsWithVotesChunks' 7
              ),
              ( "merge shards with votes in chunks of 2 succeeds (stats for 2)",
                create >>= mergeShardsWithVotesChunks' 2
              ),
              ("end proposal now succeeds", create >>= endProposal' 0),
              ( "end proposal in 100 days from start succeeds",
                create >>= endProposal' (oneDay * 100)
              ),
              ( "end proposal in 1900 days from start succeeds",
                create >>= endProposal' (oneDay * 1_900)
              ),
              ( "end proposal in 3000 days from start succeeds",
                create >>= endProposal' (oneDay * 3_000)
              ),
              ( "end failed proposal now succeeds",
                create >>= endProposalFailed 0
              ),
              ( "end failed proposal in 100 days from start succeeds",
                create >>= endProposalFailed (oneDay * 100)
              ),
              ( "end failed proposal in 1900 days from start succeeds",
                create >>= endProposalFailed (oneDay * 1_900)
              ),
              ( "end failed proposal in 3000 days from start succeeds",
                create >>= endProposalFailed (oneDay * 3_000)
              ),
              ("execute succeeds", create >>= executeProposalSuccessfully)
            ]
       in setupTestBenchmarkSuite
            suiteOpts
            (fmap (uncurry check) testData)
            (fmap (uncurry (assertLimits initialFunds cfg)) testData)

    check :: String -> Run () -> TestTree
    check = checkNoFail cfg initialFunds

    checkFailWithMsg' :: String -> ErrorMsgContains -> Run () -> TestTree
    checkFailWithMsg' = checkFailWithMsg cfg initialFunds

    createUpgradeProposal ::
      AssocMap.Map Ledger.ValidatorHash Gov.UpgradePath ->
      Run CreateProposalResult
    createUpgradeProposal uPaths = do
      let cpParam =
            CreateProposalParam
              ( Gov.UpgradeProtocol
                  Gov.UpgradePaths {Gov.uId = 1, Gov.uPaths = uPaths}
              )
      runCreateProposal cpParam

    identityGovUpgrade :: Run ()
    identityGovUpgrade = do
      let govValHash = validatorHash $ govScript Mock.govParams
          symbol =
            upgradePolicyCurrency
              govValHash
              govValHash
              identityDatumUpgradeCompiled
      createUpgradeProposal
        ( AssocMap.singleton govValHash $
            Gov.UpgradePath {Gov.upgradeSymbol = symbol}
        )
        >>= executeProposalSuccessfully
      runGovUpgrade Mock.govParams id identityDatumUpgradeCompiled

    identityCollectorUpgrade :: Run ()
    identityCollectorUpgrade = do
      let collectorValHash = validatorHash $ collectorScript Mock.collectorParams
          collectorSymbol =
            upgradePolicyCurrency
              collectorValHash
              collectorValHash
              identityDatumUpgradeCompiled

      createUpgradeProposal
        ( AssocMap.fromList [(collectorValHash, Gov.UpgradePath {Gov.upgradeSymbol = collectorSymbol})]
        )
        >>= executeProposalSuccessfully
      runCollectorUpgrade

    versionIncrementGovUpgrade :: Run ()
    versionIncrementGovUpgrade = do
      let govValHash = validatorHash $ govScript Mock.govParams
          symbol =
            upgradePolicyCurrency
              govValHash
              govValHash
              versionIncrementGovDatumUpgradeCompiled
      createUpgradeProposal
        ( AssocMap.singleton
            govValHash
            $ Gov.UpgradePath {Gov.upgradeSymbol = symbol}
        )
        >>= executeProposalSuccessfully
      runGovUpgrade
        Mock.govParams
        versionIncrementGovDatumUpgrade
        versionIncrementGovDatumUpgradeCompiled

    paramsGovUpgrade :: Run ()
    paramsGovUpgrade = do
      let newGovParams = Mock.govParams {Gov.gBiasTime = Ledger.POSIXTime 3_000}
          oldGovValHash = validatorHash $ govScript Mock.govParams
          newGovValHash = validatorHash $ govScript newGovParams
          symbol =
            upgradePolicyCurrency
              oldGovValHash
              newGovValHash
              identityDatumUpgradeCompiled
      createUpgradeProposal
        ( AssocMap.singleton oldGovValHash $
            Gov.UpgradePath {Gov.upgradeSymbol = symbol}
        )
        >>= executeProposalSuccessfully
      runGovUpgrade newGovParams id identityDatumUpgradeCompiled

    createTextProposal :: Run CreateProposalResult
    createTextProposal = do
      let cpParam =
            CreateProposalParam (Gov.TextProposal "QmYwqCVTsU7SLaZLUc2pQCq641uEHxT1EeNUAWoLYZcK5d")
      runCreateProposal cpParam

    createModifyProtocolParamsProposal :: Run CreateProposalResult
    createModifyProtocolParamsProposal =
      runCreateProposal
        ( CreateProposalParam
            ( Gov.ModifyProtocolParams
                Mock.protocolParams {Gov.expirationPeriod = 11_000}
            )
        )

    createShards' :: CreateProposalResult -> Run ()
    createShards' (pId, endTime, totalShards) = do
      runCreateShards (CreateShardsParam pId endTime totalShards)

    createShardsChunks' :: Integer -> CreateProposalResult -> Run ()
    createShardsChunks' chunkSize (pId, endTime, totalShards) = do
      runCreateShardsChunks
        chunkSize
        (CreateShardsParam pId endTime totalShards)

    vote' :: CreateProposalResult -> Run ()
    vote' (pId, endTime, totalShards) = do
      runCreateShards (CreateShardsParam pId endTime totalShards)
      waitNSlots 1
      admin <- getMainUser
      runVote admin (VoteParam pId Poll.Yes)

    mergeShardsWithVotes' :: CreateProposalResult -> Run ()
    mergeShardsWithVotes' (pId, endTime, totalShards) = do
      runCreateShards (CreateShardsParam pId endTime totalShards)
      waitNSlots 1
      admin <- getMainUser
      runVote admin (VoteParam pId Poll.Yes)
      waitNSlots 10
      runMergeShards (MergeShardsParam pId)

    mergeShardsExploit :: CreateProposalResult -> Run ()
    mergeShardsExploit (pId1, endTime1, totalShards1) = do
      -- create 2nd proposal for the exploit.
      (pId2, _, _) <- createTextProposal
      runCreateShards (CreateShardsParam pId1 endTime1 totalShards1)
      waitNSlots 1
      admin <- getMainUser
      runVote admin (VoteParam pId1 Poll.Yes)
      waitNSlots 10
      runMergeShardsExploit (MergeShardsParam pId1) (MergeShardsParam pId2)

    mergeShardsWithVotesChunks' :: Int -> CreateProposalResult -> Run ()
    mergeShardsWithVotesChunks' chunkLen (pId, endTime, totalShards) = do
      runCreateShards (CreateShardsParam pId endTime totalShards)
      waitNSlots 1
      admin <- getMainUser
      runVote admin (VoteParam pId Poll.Yes)
      waitNSlots 10
      runMergeShardsChunks chunkLen (MergeShardsParam pId)

    mergeShards' :: CreateProposalResult -> Run ()
    mergeShards' (pId, endTime, totalShards) = do
      runCreateShards (CreateShardsParam pId endTime totalShards)
      waitNSlots 10
      runMergeShards (MergeShardsParam pId)

    endProposal' :: POSIXTime -> CreateProposalResult -> Run ()
    endProposal' timeShift (pId, endTime, totalShards) = do
      runCreateShards (CreateShardsParam pId endTime totalShards)
      waitNSlots 1
      admin <- getMainUser
      runVote admin (VoteParam pId Poll.Yes)
      waitNSlots 10
      runMergeShards (MergeShardsParam pId)
      waitNSlots 1
      runEndProposal timeShift (EndParam pId totalShards)

    endProposalFailed :: POSIXTime -> CreateProposalResult -> Run ()
    endProposalFailed timeShift (pId, endTime, totalShards) = do
      runCreateShards (CreateShardsParam pId endTime totalShards)
      waitNSlots 1
      admin <- getMainUser
      runVote admin (VoteParam pId Poll.No)
      waitNSlots 10
      runMergeShards (MergeShardsParam pId)
      waitNSlots 1
      runEndProposal timeShift (EndParam pId totalShards)

initialFunds :: Value
initialFunds =
  Ada.adaValueOf 10_000_000
    <> unitValue (GovParams.govNFT Mock.govParams)
    <> Value.assetClassValue
      (GovParams.indyAsset Mock.govParams)
      100_000_000_000
    <> OracleBenchmark.initialFunds
    <> unitValue (StakingParams.stakingManagerNFT Mock.stakeParams)
    <> unitValue (CDPCommon.cdpCreatorNft Mock.cdpCreatorParams)
