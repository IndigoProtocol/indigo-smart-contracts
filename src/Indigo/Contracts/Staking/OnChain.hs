-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.Staking.OnChain
  ( validateStaking,
    stakingValidator,
    stakingAddress,
    stakingScriptCTL,
    untypedStakingValidatorHash,
  )
where

import Indigo.Contracts.Governance.Poll.Common (PollRedeemer (Vote))
import Indigo.Contracts.Governance.VersionRegistry.Common
  ( validateUpgradeVersion,
  )
import Indigo.Contracts.Staking.Common
  ( RewardSnapshot,
    StakingDatum (StakingManager, StakingPosition, mSnapshot, totalStake),
    StakingParams
      ( StakingParams,
        collectorValHash,
        indyToken,
        pollToken,
        stakingManagerNFT,
        stakingToken,
        versionRecordToken
      ),
    StakingRedeemer
      ( AdjustStakedAmount,
        CreateStakingPosition,
        Distribute,
        Lock,
        Unlock,
        Unstake,
        UpdateTotalStake,
        UpgradeVersion
      ),
    distributeReward,
    getReward,
    removeExpiredLockedAmount,
  )
import Indigo.Utils.Helpers qualified as Helpers
import Ledger qualified
import Ledger.Ada qualified as Ada
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
import PlutusTx.Builtins (divideInteger)
import PlutusTx.List qualified as List
import PlutusTx.Prelude hiding (divide)

{-# INLINEABLE validateStaking #-}
validateStaking ::
  StakingParams ->
  StakingDatum ->
  StakingRedeemer ->
  V2.ScriptContext ->
  Bool
validateStaking
  params
  (StakingManager ts snapshot)
  (CreateStakingPosition pkh)
  ctx =
    validateCreateStakingPosition params ctx ts snapshot pkh
validateStaking params StakingManager {} UpdateTotalStake ctx =
  validateUpdateTotalStake params ctx
validateStaking params (StakingManager ts snapshot) Distribute ctx =
  validateDistribute params ctx ts snapshot
validateStaking
  params
  (StakingPosition pk locked positionSnapshot)
  (AdjustStakedAmount amt)
  ctx =
    validateAdjustStakedAmount params ctx pk locked positionSnapshot amt
validateStaking
  params
  (StakingPosition pk locked positionSnapshot)
  Unstake
  ctx =
    validateUnstake params ctx pk locked positionSnapshot
validateStaking params StakingPosition {} Lock ctx = validateLock params ctx
validateStaking params (StakingPosition pk locked positionSnapshot) Unlock ctx =
  validateUnlock params ctx pk locked positionSnapshot
validateStaking StakingParams {versionRecordToken} _ UpgradeVersion ctx =
  validateUpgradeVersion "Staking" ctx versionRecordToken
validateStaking _ _ _ _ = False

{-# INLINEABLE validateCreateStakingPosition #-}
validateCreateStakingPosition ::
  StakingParams ->
  V2.ScriptContext ->
  Integer ->
  RewardSnapshot ->
  Ledger.PaymentPubKeyHash ->
  Bool
validateCreateStakingPosition
  StakingParams {stakingToken, indyToken, stakingManagerNFT}
  ctx
  ts
  snapshot
  pk =
    traceIfFalse "Invalid input being validated" (ownRef == stakeManagerRef)
      && traceIfFalse
        "Transaction must be signed by owner of staking position"
        (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pk))
      && traceIfFalse
        "Must mint exactly 1 stakingToken"
        (V2.txInfoMint info == Helpers.unitValue stakingToken)
      && traceIfFalse
        "Incorrect StakingManager output"
        ( Helpers.checkOwnOutput @StakingDatum
            ctx
            (StakingManager (ts + stakeAmount) snapshot)
            stakeManagerValue
        )
      && traceIfFalse
        "Incorrect StakingPosition output"
        ( Helpers.checkOwnOutputAdaGeq @StakingDatum
            ctx
            (StakingPosition pk PtMap.empty snapshot)
            ( Helpers.unitValue stakingToken
                <> Value.assetClassValue indyToken stakeAmount
            )
        )
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      stakeAmount :: Integer
      stakeAmount =
        Value.assetClassValueOf
          ( V2.txOutValue $
              Helpers.findUniqueOutputFromCurrentScript stakingToken ctx
          )
          indyToken

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      stakeManagerRef :: V2.TxOutRef
      stakeManagerInput :: V2.TxOut
      (stakeManagerRef, stakeManagerInput) =
        Helpers.findUniqueInputWithTokenRef stakingManagerNFT info

      stakeManagerValue :: Ledger.Value
      stakeManagerValue = V2.txOutValue stakeManagerInput

{-# INLINEABLE validateUpdateTotalStake #-}
validateUpdateTotalStake :: StakingParams -> V2.ScriptContext -> Bool
validateUpdateTotalStake StakingParams {stakingToken} ctx =
  traceIfFalse
    "Must adjust position or unstake"
    ( case Helpers.spendRedeemer info stakingPositionRef of
        AdjustStakedAmount _ -> True
        Unstake -> True
        _ -> False
    )
    && traceIfFalse
      "Mixed versions"
      (V2.txOutAddress stakingPositionInput == ownAddress)
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    stakingPositionRef :: V2.TxOutRef
    stakingPositionInput :: V2.TxOut
    (stakingPositionRef, stakingPositionInput) =
      Helpers.findUniqueInputWithTokenRef stakingToken info

    ownAddress :: V2.Address
    ownAddress = Ledger.scriptHashAddress $ Contexts.ownHash ctx

--   a = Value added to staking manager
--   b = ada coming from collector
--   c = ada going to collector
--   a = wealth which goes to snapshot
--   a = b - c
--   b = a + c
{-# INLINEABLE validateDistribute #-}
validateDistribute ::
  StakingParams ->
  V2.ScriptContext ->
  Integer ->
  RewardSnapshot ->
  Bool
validateDistribute
  StakingParams {stakingManagerNFT, collectorValHash}
  ctx
  ts
  snapshot =
    traceIfFalse "Invalid input being validated" (ownRef == stakeManagerRef)
      && traceIfFalse "Can not mint any token" (V2.txInfoMint info == mempty)
      && traceIfFalse "No INDY stakers" (ts > 0)
      {- This check serves a double purpose:

           - Purpose 1: to avoid diluting a UTxO holding a significant amount of
             fees into many UTxOs each holding a small value (which could delay
             fees distribution).

           - Purpose 2: to avoid attaching a staking credential to the UTxO
             locked back in the collector script.
      -}
      && traceIfFalse
        "Fail to return the same number of UTxOs to the collector address"
        ( numFeeOutputs == numFeeInputs
            && numFeeOutputs == numFeeOutputsNoStakingCredential
        )
      {- This check serves a double purpose:

           - Purpose 1: to avoid contention on the distribution of fees by
             setting a minimum amount to be distributed.

           - Purpose 2: to avoid locking a significant amount of fees in the
             collector due to a dust attack.
      -}
      && traceIfFalse
        "More than 95% of the distributable value has to be distributed"
        (totalFeeOutputLovelaces < totalFeeInputLovelaces `divideInteger` 20)
      && traceIfFalse
        "Incorrect StakingManager output"
        ( Helpers.checkOwnOutputAdaGeq @StakingDatum
            ctx
            (StakingManager ts newSnapShot)
            ( V2.txOutValue stakeManagerInput
                <> Ada.lovelaceValueOf adaToDistribute
            )
        )
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      feeInputs :: [Ledger.Value]
      feeInputs =
        V2.txOutValue <$> Helpers.findAllInputsFromScript collectorValHash info

      numFeeInputs :: Integer
      numFeeInputs = length feeInputs

      feeInputLoveLaces :: [Integer]
      feeInputLoveLaces = Ada.getLovelace . Ada.fromValue <$> feeInputs

      totalFeeInputLovelaces :: Integer
      totalFeeInputLovelaces = sum feeInputLoveLaces

      feeOutputs :: [(V2.OutputDatum, Ledger.Value)]
      feeOutputs = Contexts.scriptOutputsAt collectorValHash info

      totalFeeOutputLovelaces :: Integer
      totalFeeOutputLovelaces =
        sum $ Ada.getLovelace . Ada.fromValue . snd <$> feeOutputs

      numFeeOutputs :: Integer
      numFeeOutputs = length feeOutputs

      feeOutputsNoStakingCredential :: [V2.TxOut]
      feeOutputsNoStakingCredential =
        Helpers.findAllOutputsToAddress collectorValHash info

      numFeeOutputsNoStakingCredential :: Integer
      numFeeOutputsNoStakingCredential = length feeOutputsNoStakingCredential

      adaToDistribute :: Integer
      adaToDistribute = totalFeeInputLovelaces - totalFeeOutputLovelaces

      newSnapShot :: RewardSnapshot
      newSnapShot = distributeReward snapshot adaToDistribute ts

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      stakeManagerRef :: V2.TxOutRef
      stakeManagerInput :: V2.TxOut
      (stakeManagerRef, stakeManagerInput) =
        Helpers.findUniqueInputWithTokenRef stakingManagerNFT info

{-# INLINEABLE validateAdjustStakedAmount #-}
validateAdjustStakedAmount ::
  StakingParams ->
  V2.ScriptContext ->
  Ledger.PaymentPubKeyHash ->
  PtMap.Map Integer (Integer, Ledger.POSIXTime) ->
  RewardSnapshot ->
  Integer ->
  Bool
validateAdjustStakedAmount
  StakingParams {stakingToken, indyToken, stakingManagerNFT}
  ctx
  pk
  locked
  positionSnapshot
  amt =
    traceIfFalse "Invalid input being validated" (ownRef == stakePositionRef)
      && traceIfFalse
        "Mixed versions"
        (V2.txOutAddress stakeManagerInput == ownAddress)
      && traceIfFalse
        "Staking manager is not consumed correctly"
        (Helpers.usesSpendRedeemer info stakeManagerRef UpdateTotalStake)
      && traceIfFalse
        "The transaction is not signed by owner"
        (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pk))
      && traceIfFalse
        "Exceed maximum withdrawal amount"
        (currentStake + amt >= totalLockedAmount)
      && traceIfFalse
        "Incorrect StakingManager output"
        ( Helpers.checkOwnOutput @StakingDatum
            ctx
            (StakingManager (totalStake + amt) mSnapshot)
            (V2.txOutValue stakeManagerInput <> negate reward)
        )
      && traceIfFalse "Can not mint any token" (V2.txInfoMint info == mempty)
      && traceIfFalse
        "Incorrect StakingPosition output"
        ( Helpers.checkOwnOutput @StakingDatum
            ctx
            (StakingPosition pk locked mSnapshot)
            ( V2.txOutValue stakePositionInput
                <> Value.assetClassValue indyToken amt
            )
        )
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      totalLockedAmount :: Integer
      totalLockedAmount = List.foldr (max . fst) 0 (PtMap.elems locked)

      stakeManagerRef :: V2.TxOutRef
      stakeManagerInput :: V2.TxOut
      (stakeManagerRef, stakeManagerInput) =
        Helpers.findUniqueInputWithTokenRef stakingManagerNFT info

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      ownAddress :: V2.Address
      ownAddress = Ledger.scriptHashAddress $ Contexts.ownHash ctx

      stakePositionRef :: V2.TxOutRef
      stakePositionInput :: V2.TxOut
      (stakePositionRef, stakePositionInput) =
        Helpers.findUniqueInputWithTokenRef stakingToken info

      currentStake :: Integer
      currentStake =
        Value.assetClassValueOf
          (Helpers.valueWithin $ Helpers.findOwnInput' ctx)
          indyToken

      StakingManager {totalStake, mSnapshot} =
        Helpers.findInlinedDatumFromOutput stakeManagerInput

      reward :: Ledger.Value
      reward = getReward positionSnapshot mSnapshot currentStake

{-# INLINEABLE validateUnstake #-}
validateUnstake ::
  StakingParams ->
  V2.ScriptContext ->
  Ledger.PaymentPubKeyHash ->
  PtMap.Map Integer (Integer, Ledger.POSIXTime) ->
  RewardSnapshot ->
  Bool
validateUnstake
  StakingParams {stakingManagerNFT, stakingToken, indyToken}
  ctx
  pk
  lockedAmount
  positionSnapshot =
    traceIfFalse "Invalid input being validated" (ownRef == stakePositionRef)
      && traceIfFalse
        "Mixed versions"
        (V2.txOutAddress stakeManagerInput == ownAddress)
      && traceIfFalse
        "Staking manager is not consumed correctly"
        (Helpers.usesSpendRedeemer info stakeManagerRef UpdateTotalStake)
      && traceIfFalse
        "The transaction is not signed by owner"
        (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pk))
      && traceIfFalse "Can not unstake locked fund" (PtMap.null lockedAmount)
      && traceIfFalse
        "Must burn 1 stakingToken"
        (V2.txInfoMint info == inv (Helpers.unitValue stakingToken))
      && traceIfFalse
        "Incorrect StakingManager output"
        ( Helpers.checkOwnOutput @StakingDatum
            ctx
            (stakingManagerDatum {totalStake = totalStake - currentStake})
            (V2.txOutValue stakeManagerInput <> negate reward)
        )
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      ownAddress :: V2.Address
      ownAddress = Ledger.scriptHashAddress $ Contexts.ownHash ctx

      stakePositionRef :: V2.TxOutRef
      stakePositionInput :: V2.TxOut
      (stakePositionRef, stakePositionInput) =
        Helpers.findUniqueInputWithTokenRef stakingToken info

      currentStake :: Integer
      currentStake =
        Value.assetClassValueOf (V2.txOutValue stakePositionInput) indyToken

      stakeManagerRef :: V2.TxOutRef
      stakeManagerInput :: V2.TxOut
      (stakeManagerRef, stakeManagerInput) =
        Helpers.findUniqueInputWithTokenRef stakingManagerNFT info

      stakingManagerDatum@StakingManager {totalStake, mSnapshot} =
        Helpers.findInlinedDatumFromOutput stakeManagerInput

      reward :: Ledger.Value
      reward = getReward positionSnapshot mSnapshot currentStake

{-# INLINEABLE validateLock #-}
validateLock :: StakingParams -> V2.ScriptContext -> Bool
validateLock StakingParams {pollToken} ctx =
  traceIfFalse
    "Must vote"
    ( case Helpers.spendRedeemer info pollRef of
        Vote _ -> True
        _ -> False
    )
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    pollRef :: V2.TxOutRef
    (pollRef, _) = Helpers.findUniqueInputWithTokenRef pollToken info

{-# INLINEABLE validateUnlock #-}
validateUnlock ::
  StakingParams ->
  V2.ScriptContext ->
  Ledger.PaymentPubKeyHash ->
  PtMap.Map Integer (Integer, Ledger.POSIXTime) ->
  RewardSnapshot ->
  Bool
validateUnlock StakingParams {stakingToken} ctx pk locked positionSnapshot =
  traceIfFalse "Invalid input being validated" (ownRef == stakePositionRef)
    && traceIfFalse "Can not mint any token" (V2.txInfoMint info == mempty)
    && traceIfFalse
      "The transaction is not signed by owner"
      (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pk))
    && traceIfFalse
      "Incorrect StakingPosition output"
      ( Helpers.checkOwnOutput @StakingDatum
          ctx
          (StakingPosition pk locked' positionSnapshot)
          (V2.txOutValue stakePositionInput)
      )
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    ownRef :: V2.TxOutRef
    ownRef = case V2.scriptContextPurpose ctx of
      Contexts.Spending ref -> ref
      _ -> traceError "Must spend UTxO"

    stakePositionRef :: V2.TxOutRef
    stakePositionInput :: V2.TxOut
    (stakePositionRef, stakePositionInput) =
      Helpers.findUniqueInputWithTokenRef stakingToken info

    locked' :: PtMap.Map Integer (Integer, Ledger.POSIXTime)
    locked' =
      removeExpiredLockedAmount
        locked
        ( \eTime ->
            Interval.lowerBound eTime
              > Interval.ivFrom (V2.txInfoValidRange info)
        )

stakingValidator :: StakingParams -> V2.Validator
stakingValidator param =
  V2.mkValidatorScript
    (compiledValidateStaking `PlutusTx.applyCode` PlutusTx.liftCode param)

compiledValidateStaking ::
  PlutusTx.CompiledCode (StakingParams -> UntypedValidator)
compiledValidateStaking =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||mkUntypedValidator . validateStaking||])

compiledUntypedValidateStaking ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidateStaking =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          mkUntypedValidator
            . validateStaking
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedStakingValidator :: BuiltinData -> V2.Validator
untypedStakingValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidateStaking
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedStakingValidatorHash :: BuiltinData -> V2.ValidatorHash
untypedStakingValidatorHash = Scripts.validatorHash . untypedStakingValidator

-- serialised for use in CTL
stakingScriptCTL :: V2.Script
stakingScriptCTL = V2.fromCompiledCode compiledUntypedValidateStaking

stakingAddress :: StakingParams -> V2.Address
stakingAddress = Address.mkValidatorAddress . stakingValidator
