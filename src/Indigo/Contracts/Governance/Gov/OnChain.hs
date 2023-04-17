-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.Governance.Gov.OnChain
  ( validateGov,
    govAddress,
    govValidator,
    govScriptCTL,
    untypedGovHash,
  )
where

import Indigo.Contracts.Governance.Gov.Common
import Indigo.Contracts.Governance.Poll.Common
  ( PollManager
      ( PollManager,
        pContent,
        pCreatedShards,
        pEndTime,
        pExpirationTime,
        pId,
        pOwner,
        pProposeEndTime,
        pProtocolVersion,
        pStatus,
        pTalliedShards,
        pTotalShards
      ),
    PollStatus (VoteCount, nNo, nYes),
  )
import Indigo.Contracts.Governance.VersionRegistry.Common
  ( validateUpgradeVersion,
  )
import Indigo.Utils.Helpers qualified as Helpers
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
import PlutusTx.Prelude hiding (divide, fromInteger)

{-# INLINEABLE validateGov #-}
validateGov :: GovParams -> GovDatum -> GovRedeemer -> V2.ScriptContext -> Bool
validateGov param govDatum (CreatePoll pTime pOwner pContent) ctx =
  validateCreateProposal param pTime pOwner pContent govDatum ctx
validateGov param _ UpgradeGov ctx = validateGovUpgrade param ctx
validateGov param _ UpgradeVersion ctx = validateGovVersionUpgrade param ctx

{-# INLINEABLE validateCreateProposal #-}
validateCreateProposal ::
  GovParams ->
  Ledger.POSIXTime ->
  Ledger.PaymentPubKeyHash ->
  ProposalContent ->
  GovDatum ->
  V2.ScriptContext ->
  Bool
validateCreateProposal
  GovParams {gBiasTime, govNFT, indyAsset, pollToken, pollManagerValHash}
  pTime
  pOwner
  pContent
  govDatum@Gov {currentProposal, currentVersion, protocolParams}
  ctx =
    traceIfFalse
      "Incorrect Governance output"
      ( Helpers.checkOwnOutputAdaGeq @GovDatum
          ctx
          govDatum {currentProposal = currentProposal + 1}
          (Helpers.unitValue govNFT)
      )
      && traceIfFalse
        "The transaction must be signed by owner"
        (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pOwner))
      && traceIfFalse
        "Proposal incompatible with current state"
        (checkExisted pContent)
      && traceIfFalse
        "Invalid pTime"
        ( Helpers.validityTimeInInterval
            info
            (Interval.interval (pTime - gBiasTime) (pTime + gBiasTime))
        )
      && traceIfFalse
        "Transaction mint exactly 1 pollToken"
        (V2.txInfoMint info == Helpers.unitValue pollToken)
      && traceIfFalse
        "Invalid Poll output"
        ( Helpers.checkOutputFromOtherScriptsAdaGeq
            info
            pollManagerValHash
            pollDatum
            pollValue
        )
      && traceIfFalse
        "TextProposal's length is <= 64"
        ( case pContent of
            TextProposal txt -> lengthOfByteString txt <= 64
            _ -> True
        )
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      checkExisted :: ProposalContent -> Bool
      checkExisted (UpgradeProtocol UpgradePaths {uId}) =
        uId == currentVersion + 1
      checkExisted _ = True

      pollEndTime :: Ledger.POSIXTime
      pollEndTime = pTime + votingPeriod protocolParams

      pollDatum :: PollManager
      pollDatum =
        PollManager
          { pContent,
            pEndTime = pollEndTime,
            pExpirationTime = pollEndTime + expirationPeriod protocolParams,
            pId = currentProposal + 1,
            pOwner,
            pStatus = VoteCount {nYes = zero, nNo = zero},
            pCreatedShards = zero,
            pTalliedShards = zero,
            pTotalShards = totalShards protocolParams,
            pProposeEndTime = pTime + proposingPeriod protocolParams,
            pProtocolVersion = currentVersion
          }

      pollValue :: Ledger.Value
      pollValue =
        Helpers.unitValue pollToken
          <> Value.assetClassValue indyAsset (proposalDeposit protocolParams)

{-# INLINEABLE validateGovUpgrade #-}
validateGovUpgrade :: GovParams -> V2.ScriptContext -> Bool
validateGovUpgrade GovParams {upgradeToken} ctx =
  traceIfFalse
    "Spend exactly 1 Upgrade Token"
    (Helpers.isSpendingUnitValue info upgradeToken)
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

validateGovVersionUpgrade :: GovParams -> V2.ScriptContext -> Bool
validateGovVersionUpgrade GovParams {versionRecordToken} ctx =
  validateUpgradeVersion "Gov" ctx versionRecordToken

govValidator :: GovParams -> V2.Validator
govValidator param =
  V2.mkValidatorScript
    (compiledValidateGov `PlutusTx.applyCode` PlutusTx.liftCode param)

compiledValidateGov :: PlutusTx.CompiledCode (GovParams -> UntypedValidator)
compiledValidateGov =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||mkUntypedValidator . validateGov||])

compiledUntypedValidateGov ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidateGov =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          mkUntypedValidator
            . validateGov
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedGovValidator :: BuiltinData -> V2.Validator
untypedGovValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidateGov
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedGovHash :: BuiltinData -> V2.ValidatorHash
untypedGovHash = Scripts.validatorHash . untypedGovValidator

-- serialised for use in CTL
govScriptCTL :: V2.Script
govScriptCTL = V2.fromCompiledCode compiledUntypedValidateGov

govAddress :: GovParams -> Ledger.Address
govAddress = Address.mkValidatorAddress . govValidator
