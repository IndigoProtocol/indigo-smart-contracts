{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Governance.Transactions
  ( CreateProposalResult,
    executeProposalSuccessfully,
    executeProposalExploitSuccessfully,
    createAssetProposal,
    createMigrateProposal,
    createDelistProposal,
    runInitialize,
    runInitGov,
    runCreateProposal,
    runCreateShards,
    runVote,
    runMergeShards,
    runEndProposal,
    runExecute,
    runGovUpgrade,
    runCollectorUpgrade,
    identityDatumUpgradeCompiled,
    versionIncrementGovDatumUpgradeCompiled,
    versionIncrementGovDatumUpgrade,
    runMergeShardsChunks,
    runCreateShardsChunks,
    runInitRefScriptGov,
    runInitRefScriptPollManager,
    runInitRefScriptPoll,
    runInitRefScriptExecute,
    runMergeShardsExploit,
  )
where

import Control.Monad (forM_, void, when)
import Data.List (unfoldr)
import Data.List.Split (chunksOf)
import GHC.Stack (HasCallStack)
import Indigo.Contracts.CDP.Common
  ( CDPDatum (IAssetDatum),
    CDPRedeemer (UpgradeAsset),
    IAsset (IAsset),
    iaMinRatio,
    iaName,
    iaPrice,
  )
import Indigo.Contracts.Collector.Common (CollectorRedeemer (UpgradeVersion))
import Indigo.Contracts.Governance.Execute.Common qualified as Execute
import Indigo.Contracts.Governance.Gov.Common
  ( GovDatum (Gov, currentProposal, currentVersion, protocolStartTime),
  )
import Indigo.Contracts.Governance.Gov.Common qualified as Gov
import Indigo.Contracts.Governance.Poll.Common qualified as Poll
import Indigo.Contracts.Governance.VersionRegistry.Common
  ( VersionRecord (VersionRecord, versionId, versionPaths),
    VersionRecordMintingPolicyRedeemer (AddRecord),
  )
import Indigo.Contracts.Oracle.Common (OracleAssetNFT)
import Indigo.Contracts.StabilityPool.Common
  ( StabilityDatum
      ( StabilityPoolDatum,
        epochToScaleToSum,
        spIAsset,
        spSnapshot
      ),
    initEpochToScaleToSumMap,
    initSPSnapshot,
  )
import Indigo.Contracts.Staking.Common qualified as Staking
import Indigo.Data.Decimal (OnChainDecimal (OnChainDecimal))
import Indigo.Utils.Helpers (getTokenName, oneSecond, unitValue)
import Indigo.Utils.Spooky qualified as Spooky
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Model
  ( DatumMode (InlineDatum),
    Run,
    Tx,
    currentTime,
    getMainUser,
    mintValue,
    payToKey,
    payToRef,
    payToScript,
    refInputInline,
    sendTx,
    signTx,
    spend,
    spendScript,
    spendScriptRef,
    userSpend,
    utxoAt,
    validateIn,
    validatorHash,
    wait,
    waitNSlots,
  )
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (mconcat, mempty, pure, ratio, (<>))
import PlutusTx.Prelude qualified as PlutusTx
import Spec.CDP.Helpers (cdpCreatorValue, findIAsset)
import Spec.CDP.Script (cdpCreatorScript, cdpScript)
import Spec.Collector.Script (collectorScript)
import Spec.Collector.Transactions (runInitCollector)
import Spec.Governance.Helpers
  ( findAllPollShardsWithId,
    findFirstPollShardWithId,
    findGov,
    findPollManagerWithId,
    findUpgradeWithId,
    findVersionRecordByValHash,
    versionRecordOutputValue,
  )
import Spec.Governance.Params
  ( CreateProposalParam (CreateProposalParam, cpContent),
    CreateShardsParam (CreateShardsParam, csEndTime, csId, csTotalShards),
    EndParam (EndParam, eId, eTotalShards),
    ExecuteParam (ExecuteParam, exId),
    MergeShardsParam (MergeShardsParam, msId),
    VoteParam (VoteParam, vId, vOption),
  )
import Spec.Governance.Script
  ( executeScript,
    govScript,
    pollManagerAddress,
    pollManagerScript,
    pollScript,
    upgradePolicy,
    versionRecordPolicy,
    versionRegistryScript,
  )
import Spec.StabilityPool.Script (stabilityPoolScript)
import Spec.Staking.Helpers (findStakingPosition)
import Spec.Staking.Params (OpenStakingPositionParam (OpenStakingPositionParam))
import Spec.Staking.Script (stakingScript)
import Spec.Staking.Transactions
  ( OpenPositionVariation (OpenSucceed),
    runInitStaking,
    runOpenStakingPos,
  )
import Spec.Treasury.Script (treasuryScript)
import Utils.Helpers (getFirstRefScript, initScriptRef, minLovelacesPerUtxo)
import Utils.MintPolicies (authPolicy)
import Utils.Mock qualified as Mock
import Prelude (Int, fail, fromIntegral, mconcat, mempty, pure, replicate, (<>))

executeProposalSuccessfully :: CreateProposalResult -> Run ()
executeProposalSuccessfully (pId, endTime, totalShards) = do
  runCreateShards (CreateShardsParam pId endTime totalShards)
  admin <- getMainUser
  runVote admin (VoteParam pId Poll.Yes)
  waitNSlots 10
  runMergeShards (MergeShardsParam pId)
  runEndProposal 0 (EndParam pId totalShards)
  waitNSlots 3
  runExecute (ExecuteParam pId)

executeProposalExploitSuccessfully :: CreateProposalResult -> CreateProposalResult -> Run ()
executeProposalExploitSuccessfully (pId1, endTime1, totalShards1) (pId2, endTime2, totalShards2) = do
  runCreateShards (CreateShardsParam pId1 endTime1 totalShards1)
  runCreateShards (CreateShardsParam pId2 endTime2 totalShards2)
  admin <- getMainUser
  runVote admin (VoteParam pId1 Poll.Yes)
  runVote admin (VoteParam pId2 Poll.Yes)
  waitNSlots 10
  runMergeShards (MergeShardsParam pId1)
  runMergeShards (MergeShardsParam pId2)
  runEndProposal 0 (EndParam pId1 totalShards1)
  runEndProposal 0 (EndParam pId2 totalShards2)
  waitNSlots 3
  runExecuteExploit (ExecuteParam pId1) (ExecuteParam pId2)

createAssetProposal ::
  Ledger.TokenName ->
  OnChainDecimal ->
  OracleAssetNFT ->
  Run CreateProposalResult
createAssetProposal token ratio oracleAssetNFT = do
  let cpParam =
        CreateProposalParam (Gov.ProposeAsset token ratio oracleAssetNFT)
  runCreateProposal cpParam

runExecuteExploit :: HasCallStack => ExecuteParam -> ExecuteParam -> Run ()
runExecuteExploit ExecuteParam {exId = exId1} ExecuteParam {exId = exId2} = do
  admin <- getMainUser
  refScriptUtxoExecute <- getFirstRefScript (executeScript Mock.executeParams)
  (orefm, om, govDatum) <- findGov

  (oref1, _, upgrade1@Execute.Upgrade {Execute.uEndTime}) <-
    findUpgradeWithId exId1 -- First text proposal
  (oref2, _, upgrade2) <-
    findUpgradeWithId exId2 -- Second text proposal, required for the exploit
  let execution =
        mconcat
          [ refInputInline orefm,
            payToKey admin (Ada.lovelaceValueOf minLovelacesPerUtxo),
            payToKey admin (Ada.lovelaceValueOf minLovelacesPerUtxo)
          ]

  now <- currentTime
  let pToken = Gov.pollToken Mock.govParams
      uToken = Gov.upgradeToken Mock.govParams
      upgradeTokenName = getTokenName uToken
      now' = now - oneSecond
      Gov.ProtocolParams {Gov.expirationPeriod} =
        Gov.protocolParams govDatum
  payValue <-
    if now - oneSecond > uEndTime + expirationPeriod
      then
        validateIn
          (Ledger.from now')
          ( payToRef
              (govScript Mock.govParams)
              (InlineDatum govDatum)
              (V2.txOutValue om)
          )
      else
        validateIn
          (Ledger.interval now' (uEndTime + expirationPeriod - oneSecond))
          execution

  let tx =
        mconcat
          [ spendScriptRef
              refScriptUtxoExecute
              (executeScript Mock.executeParams)
              oref1
              Execute.Execute
              upgrade1,
            spendScriptRef
              refScriptUtxoExecute
              (executeScript Mock.executeParams)
              oref2
              Execute.Execute
              upgrade2,
            mintValue
              (authPolicy pToken upgradeTokenName)
              ()
              (inv $ unitValue uToken),
            payValue,
            payToKey admin (unitValue uToken) -- user steals the upgrade token
          ]

  void $ signTx admin tx >>= sendTx

createMigrateProposal ::
  Ledger.TokenName ->
  OnChainDecimal ->
  OracleAssetNFT ->
  Run CreateProposalResult
createMigrateProposal token newRatio oracle = do
  let cpParam =
        CreateProposalParam (Gov.MigrateAsset token newRatio (Right oracle))
  runCreateProposal cpParam

createDelistProposal :: Ledger.TokenName -> Run CreateProposalResult
createDelistProposal token = do
  let cpParam =
        CreateProposalParam
          ( Gov.MigrateAsset
              token
              (OnChainDecimal 100_000_000)
              (Left $ OnChainDecimal 1_000_000)
          )
  runCreateProposal cpParam

runInitRefScriptGov :: Run ()
runInitRefScriptGov = initScriptRef (govScript Mock.govParams)

runInitRefScriptPollManager :: Run ()
runInitRefScriptPollManager =
  initScriptRef (pollManagerScript Mock.pollManagerParams)

runInitRefScriptPoll :: Run ()
runInitRefScriptPoll = initScriptRef (pollScript Mock.pollParams)

runInitRefScriptExecute :: Run ()
runInitRefScriptExecute = initScriptRef (executeScript Mock.executeParams)

runInitGov :: HasCallStack => Run ()
runInitGov = do
  runInitRefScriptGov
  runInitRefScriptPollManager
  runInitRefScriptPoll
  runInitRefScriptExecute
  admin <- getMainUser
  let govVal =
        unitValue (Gov.govNFT Mock.govParams)
          <> Ada.lovelaceValueOf minLovelacesPerUtxo
  sp <- spend admin govVal
  now <- currentTime
  void $ signTx admin (userSpend sp <> initTk now govVal) >>= sendTx
  where
    initTk :: Ledger.POSIXTime -> Ledger.Value -> Tx
    initTk protocolStart =
      payToRef
        (govScript Mock.govParams)
        ( InlineDatum
            Gov
              { currentProposal = 0,
                currentVersion = 0,
                Gov.protocolParams = Mock.protocolParams,
                protocolStartTime = protocolStart
              }
        )

runInitialize :: HasCallStack => Run ()
runInitialize = do
  runInitGov
  initScriptRef (cdpCreatorScript Mock.cdpCreatorParams)
  initScriptRef (cdpScript Mock.cdpParams)
  initScriptRef (stabilityPoolScript Mock.spParams)
  runInitStaking
  admin <- getMainUser
  runOpenStakingPos admin (OpenStakingPositionParam 1_000_000_000) OpenSucceed
  runInitCollector 1 Nothing
  runInitCDPCreator
  where
    runInitCDPCreator :: Run ()
    runInitCDPCreator = do
      admin <- getMainUser
      sp <- spend admin cdpCreatorValue
      void $
        signTx
          admin
          ( mconcat
              [ userSpend sp,
                payToScript
                  (cdpCreatorScript Mock.cdpCreatorParams)
                  (InlineDatum ())
                  cdpCreatorValue
              ]
          )
          >>= sendTx

-- Containts (proposal id, end of proposal voting period,
-- total amount of voting shards)
type CreateProposalResult = (Integer, Ledger.POSIXTime, Integer)

runCreateProposal :: HasCallStack => CreateProposalParam -> Run CreateProposalResult
runCreateProposal CreateProposalParam {cpContent} = do
  refScriptUtxo <- getFirstRefScript (govScript Mock.govParams)
  admin <- getMainUser
  (oref, o, govDatum) <- findGov
  now <- currentTime
  let Gov {currentProposal, Gov.protocolParams = pParam, currentVersion} =
        govDatum
      eTime = now + Gov.votingPeriod pParam
      expirationTime = eTime + Gov.expirationPeriod pParam
      proposeEndTime = now + Gov.proposingPeriod pParam
      totalShards = Gov.totalShards pParam
      pDatum =
        Poll.PollManager
          { Poll.pId = currentProposal + 1,
            Poll.pOwner = Ledger.PaymentPubKeyHash admin,
            Poll.pContent = cpContent,
            Poll.pStatus = Poll.VoteCount {Poll.nYes = 0, Poll.nNo = 0},
            Poll.pEndTime = eTime,
            Poll.pCreatedShards = 0,
            Poll.pTalliedShards = 0,
            Poll.pTotalShards = totalShards,
            Poll.pProposeEndTime = proposeEndTime,
            Poll.pExpirationTime = expirationTime,
            Poll.pProtocolVersion = currentVersion
          }
      govNFT = Gov.govNFT Mock.govParams
      pollToken = Gov.pollToken Mock.govParams
      pollTokenName = getTokenName pollToken
      indy = Gov.indyAsset Mock.govParams
      biasTime = Gov.gBiasTime Mock.govParams
      indyAmount = Value.assetClassValue indy (Gov.proposalDeposit pParam)
  sp <- spend admin (Ada.lovelaceValueOf minLovelacesPerUtxo <> indyAmount)
  tx <-
    validateIn
      (Ledger.interval (now - oneSecond) (now + biasTime - oneSecond))
      ( mconcat
          [ spendScriptRef
              refScriptUtxo
              (govScript Mock.govParams)
              oref
              (Gov.CreatePoll now (Ledger.PaymentPubKeyHash admin) cpContent)
              govDatum,
            mintValue
              (authPolicy govNFT pollTokenName)
              ()
              (unitValue pollToken),
            payToRef
              (govScript Mock.govParams)
              (InlineDatum govDatum {Gov.currentProposal = currentProposal + 1})
              (V2.txOutValue o),
            payToRef
              (pollManagerScript Mock.pollManagerParams)
              (InlineDatum pDatum)
              ( Value.assetClassValue indy (Gov.proposalDeposit pParam)
                  <> unitValue pollToken
                  <> Ada.lovelaceValueOf minLovelacesPerUtxo
              )
          ]
      )

  void $ signTx admin (userSpend sp <> tx) >>= sendTx
  pure (currentProposal + 1, eTime, totalShards)

runCreateShardsChunks :: HasCallStack => Integer -> CreateShardsParam -> Run ()
runCreateShardsChunks
  chunkSize
  CreateShardsParam {csId, csEndTime, csTotalShards} = do
    refScriptUtxoPoll <-
      getFirstRefScript (pollManagerScript Mock.pollManagerParams)
    admin <- getMainUser
    let chunks =
          unfoldr
            ( \sizeLeft ->
                let chunk = min chunkSize sizeLeft
                 in if chunk <= 0
                      then Nothing
                      else Just (chunk, sizeLeft - chunkSize)
            )
            csTotalShards
    forM_
      chunks
      $ \newShardsChunkSize -> do
        (poref, po, poll) <- findPollManagerWithId csId
        now <- currentTime
        let createdShards = Poll.pCreatedShards poll
            newPollManager =
              poll {Poll.pCreatedShards = createdShards + newShardsChunkSize}
            zeroStatus = Poll.VoteCount {Poll.nYes = 0, Poll.nNo = 0}
            shardDatum =
              Poll.PollShard
                { psId = csId,
                  psStatus = zeroStatus,
                  psEndTime = csEndTime,
                  psManagerAddress =
                    Spooky.toSpookyAddress
                      (pollManagerAddress Mock.pollManagerParams)
                }
            govNFT = Gov.govNFT Mock.govParams
            pollToken = Gov.pollToken Mock.govParams
            pollTokenName = getTokenName pollToken
            biasTime = Gov.gBiasTime Mock.govParams
        sp <-
          spend
            admin
            (Ada.lovelaceValueOf (minLovelacesPerUtxo * newShardsChunkSize))
        tx <-
          validateIn
            (Ledger.interval (now - oneSecond) (now + biasTime - oneSecond))
            ( mconcat
                ( [ mintValue
                      (authPolicy govNFT pollTokenName)
                      ()
                      (Value.assetClassValue pollToken newShardsChunkSize),
                    spendScriptRef
                      refScriptUtxoPoll
                      (pollManagerScript Mock.pollManagerParams)
                      poref
                      (Poll.CreateShards now)
                      poll,
                    payToRef
                      (pollManagerScript Mock.pollManagerParams)
                      (InlineDatum newPollManager)
                      (V2.txOutValue po)
                  ]
                    ++ replicate
                      (fromIntegral newShardsChunkSize)
                      ( payToRef
                          (pollScript Mock.pollParams)
                          (InlineDatum shardDatum)
                          ( unitValue pollToken
                              <> Ada.lovelaceValueOf minLovelacesPerUtxo
                          )
                      )
                )
            )

        void $ signTx admin (userSpend sp <> tx) >>= sendTx

-- commented out to reduce the time (and to reduce `proposingPeriod`) value
-- required to create all shards in successive chunks
-- in plutus-simple-model test
-- when (newShardsChunkSize < csTotalShards) $ waitNSlots 1

runCreateShards :: HasCallStack => CreateShardsParam -> Run ()
runCreateShards param@CreateShardsParam {csTotalShards} =
  runCreateShardsChunks csTotalShards param

runMergeShardsChunksImpl ::
  HasCallStack =>
  [[(V2.TxOutRef, V2.TxOut, Poll.PollShard)]] ->
  MergeShardsParam ->
  Run ()
runMergeShardsChunksImpl shardUtxosChunks MergeShardsParam {msId} = do
  refScriptUtxoPollManager <-
    getFirstRefScript (pollManagerScript Mock.pollManagerParams)
  refScriptUtxoPoll <- getFirstRefScript (pollScript Mock.pollParams)
  admin <- getMainUser
  let numChunks = length shardUtxosChunks
  forM_ shardUtxosChunks $ \shardUtxos -> do
    (poref, po, poll@Poll.PollManager {Poll.pStatus}) <-
      findPollManagerWithId msId
    now <- currentTime
    let shardVotes =
          PlutusTx.fold $
            map (\(_, _, Poll.PollShard {Poll.psStatus = s}) -> s) shardUtxos
        shardAda =
          PlutusTx.fold $
            map (\(_, o, _) -> Value.adaOnlyValue $ V2.txOutValue o) shardUtxos
        numShards = fromIntegral $ length shardUtxos
        newPollManager =
          poll
            { Poll.pTalliedShards = Poll.pTalliedShards poll + numShards,
              Poll.pStatus = pStatus PlutusTx.<> shardVotes
            }
        govNFT = Gov.govNFT Mock.govParams
        biasTime = Gov.gBiasTime Mock.govParams
        pollToken = Gov.pollToken Mock.govParams
        pollTokenName = getTokenName pollToken
    tx <-
      validateIn
        (Ledger.interval (now - oneSecond) (now + biasTime - oneSecond))
        ( mconcat
            ( [ mintValue
                  (authPolicy govNFT pollTokenName)
                  ()
                  (Value.assetClassValue pollToken (negate numShards)),
                spendScriptRef
                  refScriptUtxoPollManager
                  (pollManagerScript Mock.pollManagerParams)
                  poref
                  (Poll.MergeShardsManager now)
                  poll,
                payToRef
                  (pollManagerScript Mock.pollManagerParams)
                  (InlineDatum newPollManager)
                  (V2.txOutValue po <> shardAda)
              ]
                ++ map
                  ( \(oref, _, dt) ->
                      spendScriptRef
                        refScriptUtxoPoll
                        (pollScript Mock.pollParams)
                        oref
                        (Poll.MergeShards now $ Spooky.toSpookyTxOutRef poref)
                        dt
                  )
                  shardUtxos
            )
        )

    void $ signTx admin tx >>= sendTx
    when (numChunks > 1) $ waitNSlots 1

runMergeShardsChunks :: HasCallStack => Int -> MergeShardsParam -> Run ()
runMergeShardsChunks chunkLen msp@MergeShardsParam {msId} = do
  shardUtxos <- findAllPollShardsWithId msId
  let chunks = chunksOf chunkLen shardUtxos
  runMergeShardsChunksImpl chunks msp

runMergeShards :: HasCallStack => MergeShardsParam -> Run ()
runMergeShards msp@MergeShardsParam {msId} = do
  shardUtxos <- findAllPollShardsWithId msId
  runMergeShardsChunksImpl [shardUtxos] msp

runMergeShardsExploit ::
  MergeShardsParam ->
  MergeShardsParam ->
  Run ()
runMergeShardsExploit
  MergeShardsParam {msId = msId1}
  MergeShardsParam {msId = msId2} = do
    -- Get all the shards of 1st proposal
    shardUtxos <- findAllPollShardsWithId msId1

    admin <- getMainUser

    refScriptUtxoPollManager <-
      getFirstRefScript (pollManagerScript Mock.pollManagerParams)
    refScriptUtxoPoll <-
      getFirstRefScript (pollScript Mock.pollParams)

    -- Find poll manager of 2nd proposal.
    (poref, po, poll@Poll.PollManager {}) <- findPollManagerWithId msId2

    now <- currentTime

    let biasTime = Gov.gBiasTime Mock.govParams
        pollToken = Gov.pollToken Mock.govParams

    tx <-
      validateIn
        (Ledger.interval (now - oneSecond) (now + biasTime - oneSecond))
        ( mconcat
            ( [ spendScriptRef
                  refScriptUtxoPollManager
                  (pollManagerScript Mock.pollManagerParams)
                  poref
                  (Poll.MergeShardsManager now)
                  poll,
                payToRef
                  (pollManagerScript Mock.pollManagerParams)
                  (InlineDatum poll)
                  (V2.txOutValue po),
                -- User takes all the PollToken and it's ada.
                payToKey
                  admin
                  (Ada.lovelaceValueOf 20000000 <> Value.assetClassValue pollToken 10)
              ]
                ++ map
                  ( \(oref, _, dt) ->
                      spendScriptRef
                        refScriptUtxoPoll
                        (pollScript Mock.pollParams)
                        oref
                        (Poll.MergeShards now $ Spooky.toSpookyTxOutRef poref)
                        dt
                  )
                  shardUtxos
            )
        )
    void $ signTx admin tx >>= sendTx

runVote :: HasCallStack => Ledger.PubKeyHash -> VoteParam -> Run ()
runVote user VoteParam {vId, vOption} = do
  refScriptUtxoPoll <- getFirstRefScript (pollScript Mock.pollParams)
  refScriptUtxoStaking <- getFirstRefScript (stakingScript Mock.stakeParams)
  (oref, o, poll@Poll.PollShard {Poll.psId, Poll.psEndTime}) <-
    findFirstPollShardWithId vId
  (soref, so, stakingPosition@(Staking.StakingPosition _ sMap snapshot)) <-
    findStakingPosition (Ledger.PaymentPubKeyHash user)
  let positionOutputValue = V2.txOutValue so
      indyStakedAmount = Value.assetClassValueOf positionOutputValue indyAsset
      Poll.PollManagerParams {Poll.indyAsset} = Mock.pollManagerParams
      newPoll = Poll.vote poll vOption indyStakedAmount
      pValue = V2.txOutValue o
  tx <-
    validateIn
      (Ledger.to $ psEndTime - oneSecond)
      ( mconcat
          [ spendScriptRef
              refScriptUtxoPoll
              (pollScript Mock.pollParams)
              oref
              (Poll.Vote vOption)
              poll,
            payToRef (pollScript Mock.pollParams) (InlineDatum newPoll) pValue,
            spendScriptRef
              refScriptUtxoStaking
              (stakingScript Mock.stakeParams)
              soref
              Staking.Lock
              stakingPosition,
            payToRef
              (stakingScript Mock.stakeParams)
              ( InlineDatum $
                  Staking.StakingPosition
                    (Ledger.PaymentPubKeyHash user)
                    (AssocMap.insert psId (indyStakedAmount, psEndTime) sMap)
                    snapshot
              )
              (V2.txOutValue so)
          ]
      )
  void $ signTx user tx >>= sendTx

runEndProposal :: HasCallStack => Ledger.POSIXTime -> EndParam -> Run ()
runEndProposal timeShift EndParam {eId, eTotalShards} = do
  wait timeShift

  refScriptUtxoPoll <-
    getFirstRefScript (pollManagerScript Mock.pollManagerParams)

  admin <- getMainUser
  (orefm, _om, Gov.Gov {Gov.protocolStartTime}) <- findGov
  (oref, o, poll) <- findPollManagerWithId eId

  now <- currentTime
  let Poll.PollManager
        { Poll.pContent,
          Poll.pEndTime,
          Poll.pId,
          Poll.pStatus,
          Poll.pExpirationTime,
          Poll.pProtocolVersion
        } = poll
      passQuorum =
        Poll.pollPassQuorum
          pStatus
          protocolStartTime
          initialIndyDistribution
          totalINDYSupply
          now
          distributionSchedule
      Poll.PollManagerParams
        { Poll.distributionSchedule,
          Poll.initialIndyDistribution,
          Poll.totalINDYSupply,
          Poll.pBiasTime
        } = Mock.pollManagerParams
      proposalExpired = now > pExpirationTime
      proposalPassed = not proposalExpired && passQuorum
      upgradeDatum =
        Execute.Upgrade
          { Execute.uId = pId,
            Execute.uContent = pContent,
            Execute.uPassedTime = now,
            Execute.uEndTime = pEndTime,
            Execute.uProtocolVersion = pProtocolVersion
          }
      govNFT = Gov.govNFT Mock.govParams
      pToken = Gov.pollToken Mock.govParams
      pollTokenName = getTokenName pToken
      uToken = Gov.upgradeToken Mock.govParams
      upgradeTokenName = getTokenName uToken
      proposalDeposit =
        Value.assetClassValueOf
          (V2.txOutValue o)
          (Staking.indyToken Mock.stakeParams)
  let payNewProposal =
        if proposalPassed
          then
            mconcat
              [ payToRef
                  (executeScript Mock.executeParams)
                  (InlineDatum upgradeDatum)
                  (unitValue uToken <> Ada.lovelaceValueOf minLovelacesPerUtxo),
                mintValue
                  (authPolicy pToken upgradeTokenName)
                  ()
                  (unitValue uToken)
              ]
          else mempty
  proposalDepositConstraint <-
    payProposalDepositConstraint proposalPassed admin proposalDeposit
  tx <-
    validateIn
      (Ledger.interval (now - oneSecond) (now + pBiasTime - oneSecond))
      ( mconcat
          [ spendScriptRef
              refScriptUtxoPoll
              (pollManagerScript Mock.pollManagerParams)
              oref
              (Poll.EndPoll now)
              poll,
            refInputInline orefm,
            mintValue
              (authPolicy govNFT pollTokenName)
              ()
              (inv $ unitValue pToken),
            payNewProposal,
            proposalDepositConstraint
          ]
      )
  void $ signTx admin tx >>= sendTx
  where
    proposalAdaFromShards =
      Ada.lovelaceValueOf (minLovelacesPerUtxo * eTotalShards)
    proposalAdaFromPollManager = Ada.lovelaceValueOf minLovelacesPerUtxo

    payProposalDepositConstraint ::
      Bool -> Ledger.PubKeyHash -> Integer -> Run Tx
    payProposalDepositConstraint True usr proposalDeposit = do
      sp <- spend usr proposalAdaFromPollManager
      pure $
        mconcat
          [ payToKey
              usr
              ( Value.assetClassValue
                  (Staking.indyToken Mock.stakeParams)
                  proposalDeposit
                  <> proposalAdaFromShards
                  <> proposalAdaFromPollManager
              ),
            userSpend sp
          ]
    payProposalDepositConstraint False usr proposalDeposit = do
      pure $
        mconcat
          [ payToRef
              (treasuryScript Mock.treasuryParams)
              (InlineDatum ())
              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                  <> Value.assetClassValue
                    (Staking.indyToken Mock.stakeParams)
                    proposalDeposit
              ),
            payToKey usr proposalAdaFromShards
          ]

runExecute :: HasCallStack => ExecuteParam -> Run ()
runExecute ExecuteParam {exId} = do
  refScriptUtxoCdp <- getFirstRefScript (cdpScript Mock.cdpParams)
  refScriptUtxoGov <- getFirstRefScript (govScript Mock.govParams)
  refScriptUtxoExecute <- getFirstRefScript (executeScript Mock.executeParams)
  admin <- getMainUser
  (orefm, om, govDatum) <- findGov
  (oref, _, upgrade@Execute.Upgrade {Execute.uContent, Execute.uEndTime}) <-
    findUpgradeWithId exId
  let Execute.ExecuteParams
        { Execute.iAssetToken,
          Execute.stabilityPoolToken,
          Execute.versionRecordToken
        } = Mock.executeParams
      Gov.ProtocolParams {Gov.expirationPeriod} =
        Gov.protocolParams govDatum
      iAssetOutputValue =
        unitValue iAssetToken <> Ada.lovelaceValueOf minLovelacesPerUtxo
      spOutputValue =
        unitValue stabilityPoolToken <> Ada.lovelaceValueOf minLovelacesPerUtxo
      stabilityPoolTokenName = getTokenName stabilityPoolToken
      iAssetTokenName = getTokenName iAssetToken
      uToken = Gov.upgradeToken Mock.govParams
  execution <- case uContent of
    Gov.ProposeAsset iaName iaRatio iaParam -> do
      -- This is a hack to overcome Plutus simple model spend bug with zero ada.
      sp <- spend admin (Ada.lovelaceValueOf minLovelacesPerUtxo)
      pure
        ( mconcat
            [ refInputInline orefm,
              mintValue
                (authPolicy uToken stabilityPoolTokenName)
                ()
                (unitValue stabilityPoolToken),
              mintValue
                (authPolicy uToken iAssetTokenName)
                ()
                (unitValue iAssetToken),
              payToRef
                (cdpScript Mock.cdpParams)
                (InlineDatum $ IAssetDatum (IAsset iaName iaRatio (Right iaParam)))
                iAssetOutputValue,
              payToRef
                (stabilityPoolScript Mock.spParams)
                ( InlineDatum $
                    StabilityPoolDatum
                      { spIAsset = iaName,
                        spSnapshot = initSPSnapshot,
                        epochToScaleToSum = initEpochToScaleToSumMap
                      }
                )
                spOutputValue,
              userSpend sp
            ]
        )
    Gov.UpgradeProtocol Gov.UpgradePaths {Gov.uId, Gov.uPaths} ->
      pure
        ( mconcat
            [ spendScript
                (govScript Mock.govParams)
                orefm
                Gov.UpgradeGov
                govDatum,
              mintValue
                (versionRecordPolicy Mock.vrParams)
                AddRecord
                (unitValue versionRecordToken),
              payToRef
                versionRegistryScript
                ( InlineDatum
                    VersionRecord
                      { versionId = uId,
                        versionPaths = Gov.upgradeSymbol PlutusTx.<$> uPaths
                      }
                )
                versionRecordOutputValue,
              payToRef
                (govScript Mock.govParams)
                ( InlineDatum
                    govDatum
                      { Gov.currentVersion = Gov.currentVersion govDatum + 1
                      }
                )
                (V2.txOutValue om)
            ]
        )
    Gov.MigrateAsset iaName iaMinRatio iaOracle -> do
      (orefIAsset, oIAsset, iAsset) <- findIAsset iaName
      pure
        ( mconcat
            [ refInputInline orefm,
              spendScriptRef
                refScriptUtxoCdp
                (cdpScript Mock.cdpParams)
                orefIAsset
                UpgradeAsset
                (IAssetDatum iAsset),
              payToRef
                (cdpScript Mock.cdpParams)
                ( InlineDatum $
                    IAssetDatum (IAsset {iaName, iaMinRatio, iaPrice = iaOracle})
                )
                (V2.txOutValue oIAsset),
              payToKey admin (Ada.lovelaceValueOf minLovelacesPerUtxo)
            ]
        )
    Gov.TextProposal _ ->
      pure
        ( mconcat
            [ refInputInline orefm,
              payToKey admin (Ada.lovelaceValueOf minLovelacesPerUtxo)
            ]
        )
    Gov.ModifyProtocolParams p ->
      pure
        ( mconcat
            [ spendScriptRef
                refScriptUtxoGov
                (govScript Mock.govParams)
                orefm
                Gov.UpgradeGov
                govDatum,
              payToRef
                (govScript Mock.govParams)
                (InlineDatum (govDatum {Gov.protocolParams = p}))
                (V2.txOutValue om),
              payToKey admin (Ada.lovelaceValueOf minLovelacesPerUtxo)
            ]
        )

  now <- currentTime
  let pToken = Gov.pollToken Mock.govParams
      upgradeTokenName = getTokenName uToken
      now' = now - oneSecond
  payValue <-
    if now - oneSecond > uEndTime + expirationPeriod
      then
        validateIn
          (Ledger.from now')
          ( payToRef
              (govScript Mock.govParams)
              (InlineDatum govDatum)
              (V2.txOutValue om)
          )
      else
        validateIn
          (Ledger.interval now' (uEndTime + expirationPeriod - oneSecond))
          execution
  let tx =
        mconcat
          [ spendScriptRef
              refScriptUtxoExecute
              (executeScript Mock.executeParams)
              oref
              Execute.Execute
              upgrade,
            mintValue
              (authPolicy pToken upgradeTokenName)
              ()
              (inv $ unitValue uToken),
            payValue
          ]
  void $ signTx admin tx >>= sendTx

runCollectorUpgrade ::
  Run ()
runCollectorUpgrade = do
  let collectorValHash = validatorHash $ collectorScript Mock.collectorParams
      collectorVal = 20_000_000

  -- create multiple collector utxos with same amount ada.
  runInitCollector 3 (Just collectorVal)

  admin <- getMainUser

  (vr, _, VersionRecord {versionPaths}) <-
    findVersionRecordByValHash collectorValHash
      >>= maybe (fail "missing version record") pure

  collectorRefScriptUtxo <- getFirstRefScript (collectorScript Mock.collectorParams)
  rewardUtxos <- utxoAt collectorValHash

  let Just upgradeSymbol = AssocMap.lookup collectorValHash versionPaths
      upgradeTokenVal = Value.singleton upgradeSymbol "upgrade" 1
      tx =
        mconcat
          [ mintValue
              (upgradePolicy collectorValHash collectorValHash identityDatumUpgradeCompiled)
              ()
              upgradeTokenVal,
            refInputInline vr,
            spendScriptRef
              collectorRefScriptUtxo
              (collectorScript Mock.collectorParams)
              (fst $ rewardUtxos !! 1)
              UpgradeVersion
              (),
            payToRef
              (collectorScript Mock.collectorParams)
              (InlineDatum ())
              (V2.txOutValue $ snd $ rewardUtxos !! 1),
            payToKey admin upgradeTokenVal
          ]

  void $ signTx admin tx >>= sendTx

runGovUpgrade ::
  HasCallStack =>
  Gov.GovParams ->
  (GovDatum -> GovDatum) ->
  PlutusTx.CompiledCode (Ledger.Datum -> Ledger.Datum) ->
  Run ()
runGovUpgrade newGovParams datumUpgrade compiledDatumUpgrade = do
  refScriptUtxo <- getFirstRefScript (govScript Mock.govParams)
  admin <- getMainUser
  (gr, go, gd) <- findGov
  let Just govValHash = Ledger.toValidatorHash $ V2.txOutAddress go
  (vr, _, VersionRecord {versionPaths}) <-
    findVersionRecordByValHash govValHash
      >>= maybe (fail "missing version record") pure

  let Just upgradeSymbol = AssocMap.lookup govValHash versionPaths
      oldGovValidator = govScript Mock.govParams
      newGovValidator = govScript newGovParams
      newGovValHash = validatorHash newGovValidator
      upgradeTokenVal = Value.singleton upgradeSymbol "upgrade" 1
      tx =
        mconcat
          [ mintValue
              (upgradePolicy govValHash newGovValHash compiledDatumUpgrade)
              ()
              upgradeTokenVal,
            spendScriptRef
              refScriptUtxo
              oldGovValidator
              gr
              Gov.UpgradeVersion
              gd,
            refInputInline vr,
            payToRef
              newGovValidator
              (InlineDatum (datumUpgrade gd))
              (V2.txOutValue go),
            payToKey admin upgradeTokenVal
          ]
  void $ signTx admin tx >>= sendTx

identityDatumUpgradeCompiled ::
  PlutusTx.CompiledCode (Ledger.Datum -> Ledger.Datum)
identityDatumUpgradeCompiled = $$(PlutusTx.compile [||id||])

versionIncrementGovDatumUpgradeCompiled ::
  PlutusTx.CompiledCode (Ledger.Datum -> Ledger.Datum)
versionIncrementGovDatumUpgradeCompiled =
  $$(PlutusTx.compile [||upgradeDatum||])
  where
    upgradeDatum :: Ledger.Datum -> Ledger.Datum
    upgradeDatum (Ledger.Datum d) = case PlutusTx.fromBuiltinData d of
      Nothing -> PlutusTx.traceError "Can't decode datum"
      Just dd ->
        Ledger.Datum
          (PlutusTx.toBuiltinData (versionIncrementGovDatumUpgrade dd))

{-# INLINEABLE versionIncrementGovDatumUpgrade #-}
versionIncrementGovDatumUpgrade :: GovDatum -> GovDatum
versionIncrementGovDatumUpgrade d@Gov {Gov.currentVersion} =
  d {Gov.currentVersion = currentVersion PlutusTx.+ 1}
