-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.Governance.Poll.OnChain
  ( validatePoll,
    validatePollManager,
    pollAddress,
    pollManagerAddress,
    pollValidator,
    pollScriptCTL,
    pollManagerValidator,
    pollManagerScriptCTL,
    untypedPollValidatorHash,
    untypedPollManagerValidatorHash,
  )
where

import Indigo.Contracts.Governance.Execute.Common
  ( Upgrade
      ( Upgrade,
        uContent,
        uEndTime,
        uId,
        uPassedTime,
        uProtocolVersion
      ),
  )
import Indigo.Contracts.Governance.Gov.Common
  ( GovDatum (Gov, protocolStartTime),
  )
import Indigo.Contracts.Governance.Poll.Common
import Indigo.Contracts.Staking.Common
  ( StakingDatum (StakingPosition, lockedAmount, owner, pSnapshot),
    StakingRedeemer (Lock),
  )
import Indigo.Utils.Helpers qualified as Helpers
import Indigo.Utils.Spooky qualified as Spooky
import Indigo.Utils.Spooky.Helpers qualified as Spooky.Helpers
import Indigo.Utils.Utils qualified as Utils
import Ledger qualified
import Ledger.Interval qualified as Interval
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Address qualified as Address
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators
  ( UntypedValidator,
    mkUntypedValidator,
  )
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PtMap
import PlutusTx.Prelude hiding (divide, fromInteger)

{-# INLINEABLE validatePoll #-}
validatePoll ::
  PollParams ->
  PollShard ->
  PollRedeemer ->
  Spooky.ScriptContext ->
  Bool
validatePoll param pollShard (Vote vOption) ctx =
  validateVote param pollShard vOption ctx
validatePoll param poll (MergeShards pTime pollManagerRef) ctx =
  validateMergeShards param poll pTime pollManagerRef ctx

{-# INLINEABLE validatePollManager #-}
validatePollManager ::
  PollManagerParams ->
  PollManager ->
  PollManagerRedeemer ->
  V2.ScriptContext ->
  Bool
validatePollManager param pollManager (EndPoll pTime) ctx =
  validateEndPoll param pollManager pTime ctx
validatePollManager param pollManager (CreateShards pTime) ctx =
  validateCreateShards param pollManager pTime ctx
validatePollManager param poll (MergeShardsManager pTime) ctx =
  validateMergeShardsManager param poll pTime ctx

{-# INLINEABLE validateCreateShards #-}
validateCreateShards ::
  PollManagerParams ->
  PollManager ->
  Ledger.POSIXTime ->
  V2.ScriptContext ->
  Bool
validateCreateShards
  PollManagerParams {pollToken, pBiasTime, shardsValHash}
  poll@PollManager
    { pId,
      pCreatedShards,
      pTotalShards,
      pProposeEndTime,
      pEndTime,
      pOwner
    }
  pTime
  ctx =
    traceIfFalse "Invalid input being validated" (ownRef == pollRef)
      && traceIfFalse
        "Must mint a pollToken for each shard"
        (V2.txInfoMint info == Value.assetClassValue pollToken numShardOutputs)
      && traceIfFalse
        "Incorrect pTime"
        ( Helpers.validityTimeInInterval
            info
            (Interval.interval (pTime - pBiasTime) (pTime + pBiasTime))
        )
      && traceIfFalse
        "Can't create shard after proposeEndTime"
        (pTime < pProposeEndTime)
      && traceIfFalse
        "Num of created shards + num of new shards should be <= pTotalShards"
        (pCreatedShards + numShardOutputs <= pTotalShards)
      && traceIfFalse
        "A shard output has invalid datum"
        (all hasShardDatum shardOutputsWithPollToken)
      && traceIfFalse
        "New PollManager output can only change pCreatedShards"
        checkNewManagerOutput
      && traceIfFalse
        "Only proposal creator can create shards"
        (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pOwner))
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      pollRef :: V2.TxOutRef
      pollManagerInput :: V2.TxOut
      (pollRef, pollManagerInput) =
        Helpers.findUniqueInputWithTokenRef pollToken info

      ownAddress =
        Spooky.toSpookyAddress $ Contexts.txOutAddress pollManagerInput

      pollManagerValue = Contexts.txOutValue pollManagerInput

      shardOutputsWithPollToken = filter predicate (Contexts.txInfoOutputs info)
        where
          predicate txOut =
            Contexts.txOutAddress txOut
              == Ledger.scriptHashAddress shardsValHash
              && Helpers.isAuthOutput pollToken txOut

      hasShardDatum txOut =
        case Helpers.findInlinedDatumFromOutput' @PollShard txOut of
          Just PollShard {psId, psEndTime, psStatus, psManagerAddress} ->
            psId == pId
              && psEndTime == pEndTime
              && psStatus == mempty
              && psManagerAddress == ownAddress
          _ -> False

      numShardOutputs = length shardOutputsWithPollToken

      validNewManagerDatum =
        poll {pCreatedShards = pCreatedShards + numShardOutputs}
      validNewManagerValue = pollManagerValue
      checkNewManagerOutput =
        Helpers.checkOwnOutput @PollManager
          ctx
          validNewManagerDatum
          validNewManagerValue

{-# INLINEABLE validateMergeShards #-}
validateMergeShards ::
  PollParams ->
  PollShard ->
  Ledger.POSIXTime ->
  Spooky.TxOutRef ->
  Spooky.ScriptContext ->
  Bool
validateMergeShards
  PollParams {}
  PollShard {psManagerAddress}
  pTime
  pollManagerRef
  ctx =
    traceIfFalse
      "Should call a PollManager input with MergeShards redeemer"
      checkPollManagerInput
    where
      info :: Spooky.TxInfo
      info = Spooky.scriptContextTxInfo ctx

      checkPollManagerInput =
        PtMap.lookup
          (Spooky.Spending $ Spooky.toSpooky pollManagerRef)
          (Spooky.txInfoRedeemers info)
          == Just
            ( V2.Redeemer $
                PlutusTx.toBuiltinData (MergeShardsManager pTime)
            )
          && any lockedByManager (Spooky.txInfoInputs info)
      lockedByManager input =
        Spooky.txInInfoOutRef input == pollManagerRef
          && Spooky.txOutAddress (Spooky.txInInfoResolved input)
            == psManagerAddress

{-# INLINEABLE validateMergeShardsManager #-}
validateMergeShardsManager ::
  PollManagerParams ->
  PollManager ->
  Ledger.POSIXTime ->
  V2.ScriptContext ->
  Bool
validateMergeShardsManager
  PollManagerParams {pollToken, pBiasTime}
  poll@PollManager {pStatus = pollStatus, pEndTime, pTalliedShards, pId}
  pTime
  ctx =
    traceIfFalse
      "Poll token not present"
      (Helpers.hasUnitValue ownPollManagerInputVal pollToken)
      && traceIfFalse
        "Must burn a pollToken for each shard input"
        ( V2.txInfoMint info
            == Value.assetClassValue pollToken (negate numShardInputs)
        )
      && traceIfFalse
        "Incorrect pTime"
        ( Helpers.validityTimeInInterval
            info
            (Interval.interval (pTime - pBiasTime) (pTime + pBiasTime))
        )
      && traceIfFalse "Can't merge shards before poll end time" (pTime > pEndTime)
      && traceIfFalse "Invalid new PollManager output" checkNewManagerOutput
      && traceIfFalse
        "Can only consume numShardInputs + 1 poll manager"
        (length inputsWithPollToken == (numShardInputs + 1))
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      ownPollManagerInput = Helpers.findOwnInput' ctx
      ownPollManagerInputVal =
        Contexts.txOutValue . Contexts.txInInfoResolved $ ownPollManagerInput

      inputsWithPollToken =
        Utils.filterMap
          (Helpers.isAuthOutput pollToken)
          Contexts.txInInfoResolved
          $ Contexts.txInfoInputs info

      hasShardDatum txOut =
        case Helpers.findInlinedDatumFromOutput' @PollShard txOut of
          Just PollShard {psId} -> pId == psId
          _ -> False

      shardInputs = filter hasShardDatum inputsWithPollToken
      numShardInputs = length shardInputs

      shardsAda = foldMap (Value.adaOnlyValue . V2.txOutValue) shardInputs
      validNewManagerValue = ownPollManagerInputVal + shardsAda

      newVoteStatus =
        foldMap
          (psStatus . Helpers.findInlinedDatumFromOutput @PollShard)
          shardInputs
      validNewManagerDatum =
        poll
          { pStatus = pollStatus <> newVoteStatus,
            pTalliedShards = pTalliedShards + numShardInputs
          }

      checkNewManagerOutput =
        Helpers.checkOwnOutput @PollManager
          ctx
          validNewManagerDatum
          validNewManagerValue

{-# INLINEABLE validateVote #-}
validateVote ::
  PollParams ->
  PollShard ->
  VoteOption ->
  Spooky.ScriptContext ->
  Bool
validateVote
  PollParams {indyAsset, pollToken, stakingToken, stakingValHash}
  poll@PollShard {psEndTime, psId}
  option
  ctx =
    traceIfFalse "Invalid input being validated" (ownRef == pollRef)
      && traceIfFalse
        "The proposal exceeds voting period"
        ( Interval.upperBound psEndTime
            >= Interval.ivTo (Spooky.txInfoValidRange info)
        )
      && traceIfFalse
        "The poll output is incorrect"
        ( Spooky.Helpers.checkOwnOutput @PollShard
            ctx
            (vote poll option amount)
            pollValue
        )
      && traceIfFalse
        "Mixed versions"
        ( Spooky.toValidatorHash (Spooky.txOutAddress stakingInput)
            == Just stakingValHash
        )
      && traceIfFalse
        "The transaction must be signed by owner"
        ( Spooky.txSignedBy
            info
            ( Spooky.unPaymentPubKeyHash $
                Spooky.toSpookyPaymentPubKeyHash owner
            )
        )
      && traceIfFalse
        "User voted for poll before"
        (isNothing (PtMap.lookup psId lockedAmount))
      && traceIfFalse "Must mint no token" (Spooky.txInfoMint info == mempty)
      && traceIfFalse
        "StakingPosition must update properly"
        ( Spooky.Helpers.checkOutputFromOtherScripts
            info
            stakingValHash
            StakingPosition {lockedAmount = newLocked, owner, pSnapshot}
            stakingValue
        )
      && traceIfFalse
        "Staking Position must use Lock redeemer"
        (Spooky.Helpers.usesSpendRedeemer info stakingRef Lock)
    where
      info :: Spooky.TxInfo
      info = Spooky.scriptContextTxInfo ctx

      ownRef :: Spooky.TxOutRef
      ownRef = case Spooky.scriptContextPurpose ctx of
        Spooky.Spending ref -> Spooky.unSpooky ref
        _ -> traceError "Must spend UTxO"

      pollRef :: Spooky.TxOutRef
      pollInput :: Spooky.TxOut
      (pollRef, pollInput) =
        Spooky.Helpers.findUniqueInputWithTokenRef pollToken info

      pollValue = Spooky.txOutValue pollInput

      stakingRef :: Spooky.TxOutRef
      stakingInput :: Spooky.TxOut
      (stakingRef, stakingInput) =
        Spooky.Helpers.findUniqueInputWithTokenRef stakingToken info

      stakingValue = Spooky.txOutValue stakingInput

      StakingPosition {lockedAmount, owner, pSnapshot} =
        Spooky.Helpers.findInlinedDatumFromOutput stakingInput

      newLocked = PtMap.insert psId (amount, psEndTime) lockedAmount

      amount = Spooky.assetClassValueOf stakingValue indyAsset

{-# INLINEABLE validateEndPoll #-}
validateEndPoll ::
  PollManagerParams ->
  PollManager ->
  Ledger.POSIXTime ->
  V2.ScriptContext ->
  Bool
validateEndPoll
  PollManagerParams
    { govExecuteValHash,
      govNFT,
      indyAsset,
      pBiasTime,
      pollToken,
      upgradeToken,
      initialIndyDistribution,
      totalINDYSupply,
      distributionSchedule,
      treasuryValHash
    }
  PollManager
    { pContent,
      pEndTime,
      pId,
      pStatus,
      pTalliedShards,
      pTotalShards,
      pExpirationTime,
      pOwner,
      pProtocolVersion
    }
  pTime
  ctx =
    traceIfFalse "Invalid input being validated" (ownRef == pollRef)
      && traceIfFalse "The proposal is in voting period" (pTime > pEndTime)
      && traceIfFalse
        "Incorrect pTime"
        ( Helpers.validityTimeInInterval
            info
            (Interval.interval (pTime - pBiasTime) (pTime + pBiasTime))
        )
      && traceIfFalse
        "All shards should be merged"
        (pollExpired || pTalliedShards == pTotalShards)
      && traceIfFalse
        "Only owner can close a non-expired poll"
        ( pollExpired
            || Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pOwner)
        )
      && ( if not pollExpired && passQuorum
             then
               traceIfFalse
                 "Incorrect Upgrade output"
                 ( Helpers.checkOutputFromOtherScriptsAdaGeq
                     info
                     govExecuteValHash
                     upgradeDatum
                     (Helpers.unitValue upgradeToken)
                 )
                 && traceIfFalse
                   "Must burn 1 poll token and mint 1 update token if success"
                   ( V2.txInfoMint info
                       == inv (Helpers.unitValue pollToken)
                         <> Helpers.unitValue upgradeToken
                   )
             else
               traceIfFalse
                 "Must burn 1 poll token if fail"
                 (V2.txInfoMint info == inv (Helpers.unitValue pollToken))
                 && traceIfFalse
                   "Must pay INDY deposit to treasury if fail"
                   paysToTreasuryWithValidDatum
         )
    where
      passQuorum =
        pollPassQuorum
          pStatus
          protocolStartTime
          initialIndyDistribution
          totalINDYSupply
          pTime
          distributionSchedule

      pollExpired = pTime > pExpirationTime

      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      Gov {protocolStartTime} = Helpers.findInlinedDatumFromOutput govInput

      govInput :: V2.TxOut
      govInput = Helpers.findUniqueReferenceInputWithToken govNFT info

      paysToTreasuryWithValidDatum :: Bool
      paysToTreasuryWithValidDatum =
        Helpers.checkOutputFromOtherScriptsAdaGeq
          info
          treasuryValHash
          ()
          (Value.assetClassValue indyAsset proposalDeposit)

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      pollRef :: V2.TxOutRef
      pollInput :: V2.TxOut
      (pollRef, pollInput) = Helpers.findUniqueInputWithTokenRef pollToken info

      proposalDeposit :: Integer
      proposalDeposit =
        Value.assetClassValueOf (V2.txOutValue pollInput) indyAsset

      upgradeDatum :: Upgrade
      upgradeDatum =
        Upgrade
          { uId = pId,
            uContent = pContent,
            uEndTime = pEndTime,
            uPassedTime = pTime,
            uProtocolVersion = pProtocolVersion
          }

pollValidator :: PollParams -> V2.Validator
pollValidator param =
  V2.mkValidatorScript
    (compiledValidatePoll `PlutusTx.applyCode` PlutusTx.liftCode param)

pollManagerValidator :: PollManagerParams -> V2.Validator
pollManagerValidator param =
  V2.mkValidatorScript
    (compiledValidatePollManager `PlutusTx.applyCode` PlutusTx.liftCode param)

compiledValidatePoll :: PlutusTx.CompiledCode (PollParams -> UntypedValidator)
compiledValidatePoll =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||Spooky.mkUntypedValidator . validatePoll||])

compiledValidatePollManager ::
  PlutusTx.CompiledCode (PollManagerParams -> UntypedValidator)
compiledValidatePollManager =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||mkUntypedValidator . validatePollManager||])

compiledUntypedValidatePoll ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidatePoll =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          Spooky.mkUntypedValidator
            . validatePoll
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedPollValidator :: BuiltinData -> V2.Validator
untypedPollValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidatePoll
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedPollValidatorHash :: BuiltinData -> V2.ValidatorHash
untypedPollValidatorHash = Scripts.validatorHash . untypedPollValidator

compiledUntypedValidatePollManager ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidatePollManager =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          mkUntypedValidator
            . validatePollManager
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedPollManagerValidator :: BuiltinData -> V2.Validator
untypedPollManagerValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidatePollManager
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedPollManagerValidatorHash :: BuiltinData -> V2.ValidatorHash
untypedPollManagerValidatorHash = Scripts.validatorHash . untypedPollManagerValidator

-- serialised for use in CTL
pollScriptCTL :: V2.Script
pollScriptCTL = V2.fromCompiledCode compiledUntypedValidatePoll

-- serialised for use in CTL
pollManagerScriptCTL :: V2.Script
pollManagerScriptCTL = V2.fromCompiledCode compiledUntypedValidatePollManager

pollAddress :: PollParams -> Ledger.Address
pollAddress = Address.mkValidatorAddress . pollValidator

pollManagerAddress :: PollManagerParams -> Ledger.Address
pollManagerAddress = Address.mkValidatorAddress . pollManagerValidator
