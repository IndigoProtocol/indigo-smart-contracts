-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.StabilityPool.OnChain
  ( validateStabilityPool,
    stabilityPoolValidator,
    stabilityPoolAddress,
    stabilityPoolScriptCTL,
    untypedStabilityPoolValidatorHash,
  )
where

import Indigo.Contracts.CDP.Common (CDPDatum (CDP), cdpIAsset, protocolFee)
import Indigo.Contracts.CDP.Common qualified as CDP
import Indigo.Contracts.Governance.VersionRegistry.Common
  ( validateUpgradeVersion,
  )
import Indigo.Contracts.Helpers as ContractHelpers
import Indigo.Contracts.StabilityPool.Common
import Indigo.Data.Decimal (OnChainDecimal)
import Indigo.Utils.Helpers qualified as Helpers
import Ledger qualified
import Ledger.Ada qualified as Ada
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
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (scale)

stabilityPoolValidator :: StabilityPoolParams -> V2.Validator
stabilityPoolValidator param =
  V2.mkValidatorScript
    (compiledValidateSP `PlutusTx.applyCode` PlutusTx.liftCode param)

compiledValidateSP ::
  PlutusTx.CompiledCode (StabilityPoolParams -> UntypedValidator)
compiledValidateSP =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||mkUntypedValidator . validateStabilityPool||])

compiledUntypedValidateSP ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidateSP =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          mkUntypedValidator
            . validateStabilityPool
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedStabilityPoolValidator :: BuiltinData -> V2.Validator
untypedStabilityPoolValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidateSP
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedStabilityPoolValidatorHash :: BuiltinData -> V2.ValidatorHash
untypedStabilityPoolValidatorHash = Scripts.validatorHash . untypedStabilityPoolValidator

-- serialised for use in CTL
stabilityPoolScriptCTL :: V2.Script
stabilityPoolScriptCTL = V2.fromCompiledCode compiledUntypedValidateSP

{-# INLINEABLE validateStabilityPool #-}
validateStabilityPool ::
  StabilityPoolParams ->
  StabilityDatum ->
  StabilityPoolRedeemer ->
  V2.ScriptContext ->
  Bool
validateStabilityPool params StabilityPoolDatum {..} CreateAccount {..} ctx =
  validateCreateAccount
    params
    spIAsset
    spSnapshot
    epochToScaleToSum
    caPkh
    caAmount
    ctx
validateStabilityPool params StabilityPoolDatum {..} AdjustAccount {aaDepositChange} ctx =
  validateAdjustAccount
    params
    spIAsset
    spSnapshot
    epochToScaleToSum
    aaDepositChange
    ctx
validateStabilityPool params StabilityPoolDatum {..} LiquidateCDP ctx =
  validateLiquidateCDP params spIAsset spSnapshot epochToScaleToSum ctx
validateStabilityPool params StabilityPoolDatum {..} Close ctx =
  validateClose params spIAsset spSnapshot epochToScaleToSum ctx
validateStabilityPool
  params
  StabilityPoolDatum {..}
  RecordEpochToScaleToSum
  ctx =
    validateRecordEpochToScaleToSum
      params
      spIAsset
      spSnapshot
      epochToScaleToSum
      ctx
validateStabilityPool params AccountDatum {} SpendAccount ctx =
  validateSpendAccount (stabilityPoolToken params) ctx
validateStabilityPool
  StabilityPoolParams {versionRecordToken}
  _
  UpgradeVersion
  ctx =
    validateUpgradeVersion "StabilityPool" ctx versionRecordToken
validateStabilityPool _ _ _ _ = False

{-# INLINEABLE validateRecordEpochToScaleToSum #-}
validateRecordEpochToScaleToSum ::
  StabilityPoolParams ->
  Ledger.TokenName ->
  StabilityPoolSnapshot ->
  EpochToScaleToSum ->
  V2.ScriptContext ->
  Bool
validateRecordEpochToScaleToSum
  StabilityPoolParams {stabilityPoolToken, snapshotEpochToScaleToSumToken}
  asset
  snapshot
  epochToScaleToSum
  ctx =
    traceIfFalse "Invalid input being validated" (ownRef == poolRef)
      && traceIfFalse
        "Correct SP output"
        ( Helpers.checkOwnOutput @StabilityDatum
            ctx
            ( StabilityPoolDatum
                { spIAsset = asset,
                  spSnapshot = snapshot,
                  epochToScaleToSum = AssocMap.fromList [remainingMapItem]
                }
            )
            (V2.txOutValue poolInput)
        )
      && traceIfFalse
        "Mints a single epochToScaleToSum token"
        ( V2.txInfoMint info
            == Helpers.unitValue snapshotEpochToScaleToSumToken
        )
      && traceIfFalse
        "Correct snapshot epochToScaleToSum output"
        ( Helpers.checkOwnOutput @StabilityDatum
            ctx
            ( SnapshotEpochToScaleToSumDatum
                { sessSnapshot = AssocMap.fromList snapshotMapItems,
                  sessAsset = asset
                }
            )
            (Helpers.unitValue snapshotEpochToScaleToSumToken)
        )
      && traceIfFalse "snapshot map is not empty" (not (null snapshotMapItems))
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      (remainingMapItem, snapshotMapItems) =
        partitionEpochToScaleToSumMap snapshot epochToScaleToSum

      poolRef :: V2.TxOutRef
      poolInput :: V2.TxOut
      (poolRef, poolInput) =
        Helpers.findUniqueInputWithTokenRef stabilityPoolToken info

{-# INLINEABLE validateSpendAccount #-}
validateSpendAccount :: Ledger.AssetClass -> V2.ScriptContext -> Bool
validateSpendAccount spToken ctx =
  traceIfFalse "Mixed versions" (spValHash == Contexts.ownHash ctx)
    && traceIfFalse
      "Must consume Stability Pool correctly"
      ( case Helpers.spendRedeemer info spInputRef of
          AdjustAccount _ -> True
          Close -> True
          _ -> False
      )
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    spInputRef :: V2.TxOutRef
    spInput :: V2.TxOut
    (spInputRef, spInput) = Helpers.findUniqueInputWithTokenRef spToken info

    spValHash :: Ledger.ValidatorHash
    spValHash = case Ledger.toValidatorHash (V2.txOutAddress spInput) of
      Just valHash -> valHash
      Nothing -> traceError "Could not get validator hash"

{-# INLINEABLE validateLiquidateCDP #-}
validateLiquidateCDP ::
  StabilityPoolParams ->
  Ledger.TokenName ->
  StabilityPoolSnapshot ->
  EpochToScaleToSum ->
  V2.ScriptContext ->
  Bool
validateLiquidateCDP
  StabilityPoolParams {..}
  asset
  snapshot
  epochToScaleToSum
  ctx =
    -- This check prevents protocol blockade for SP actions.
    -- EpochToScaleToSum snapshot needs to be created to process further.
    traceIfFalse
      "EpochToScaleToSum map is too large, use RecordEpochToScaleToSum"
      (length (AssocMap.toList epochToScaleToSum) <= 5)
      && traceIfFalse
        "iAsset of stability pool must match iAsset of cdp"
        (asset == cdpIAsset)
      && traceIfFalse
        "CDP input must be liquidated"
        (Helpers.usesSpendRedeemer info cdpRef CDP.Liquidate)
      && traceIfFalse
        "StabilityPool output missing"
        (Helpers.hasUnitValue spInputValue stabilityPoolToken)
      && traceIfFalse
        "Incorrect Stability Pool Output"
        ( Helpers.checkOutputDatum
            (StabilityPoolDatum asset poolSnapshot newEpochToScaleToSum)
            spOutput
        )
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      spInputValue :: Ledger.Value
      spInputValue = Helpers.valueWithin $ Helpers.findOwnInput' ctx

      spOutput :: V2.TxOut
      [spOutput] = Contexts.getContinuingOutputs ctx

      spOutputValue :: Ledger.Value
      spOutputValue = V2.txOutValue spOutput

      cdpInput :: V2.TxOut
      cdpRef :: V2.TxOutRef
      (cdpRef, cdpInput) =
        Helpers.findUniqueInputWithPositiveAmtOfTokensRef cdpToken info

      CDP {cdpIAsset} = Helpers.findInlinedDatumFromOutput cdpInput

      assetBurn :: Integer
      assetBurn = negate $ Value.valueOf (V2.txInfoMint info) assetSymbol asset

      collateralAmount :: Integer
      collateralAmount =
        Helpers.lovelacesAmount spOutputValue
          - Helpers.lovelacesAmount spInputValue

      poolSnapshot :: StabilityPoolSnapshot
      newEpochToScaleToSum :: EpochToScaleToSum
      (poolSnapshot, newEpochToScaleToSum) =
        liquidateHelper snapshot epochToScaleToSum assetBurn collateralAmount

{-# INLINEABLE validateClose #-}
validateClose ::
  StabilityPoolParams ->
  Ledger.TokenName ->
  StabilityPoolSnapshot ->
  EpochToScaleToSum ->
  V2.ScriptContext ->
  Bool
validateClose
  params@StabilityPoolParams {..}
  asset
  snapshot
  epochToScaleToSum
  ctx =
    traceIfFalse "Invalid input being validated" (ownRef == poolRef)
      && traceIfFalse
        "Wrong signature"
        (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pk))
      && traceIfFalse "The asset does not match" (asset == aAsset)
      && traceIfFalse
        "Must burn the account token"
        (V2.txInfoMint info == Value.assetClassValue accountToken (-1))
      && traceIfFalse
        "Expected to consume exactly one Account output"
        (Helpers.isSpendingUnitValue info accountToken)
      && traceIfFalse
        "Stability Pool output does not match"
        ( Helpers.checkOwnOutput
            ctx
            (StabilityPoolDatum asset poolSnapshot epochToScaleToSum)
            $ poolValue
              <> Ada.lovelaceValueOf (negate reward)
              <> V2.singleton assetSymbol asset (negate . fromSPInteger $ fund)
        )
      && traceIfFalse
        "Must pay protocol fee correctly"
        (ContractHelpers.payProtocolFeeCorrectly collectorValHash info fee)
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      StabilityPoolSnapshot p d s snapshotEpoch snapshotScale = snapshot

      AccountDatum pk aAsset aSnapshot =
        Helpers.findInlinedDatumFromOutput
          (Helpers.findUniqueInputWithToken accountToken info)

      reward :: Integer
      fund :: SPInteger
      (fund, reward) =
        case fromAdjustHelper of
          (adjustedSnap, Just rew) -> (snapshotD adjustedSnap, rew)
          (_, Nothing) -> traceError "EpochToScaleToSum map missing item"
        where
          fromAdjustHelper =
            adjustAccountHelper
              snapshot
              aSnapshot
              ( accumulatedEpochToScaleToSum
                  params
                  asset
                  epochToScaleToSum
                  info
              )

      protocolFeePercentage :: OnChainDecimal
      protocolFeePercentage = getProtocolFeePercentage govNFT info

      fee :: Integer
      fee = protocolFee protocolFeePercentage reward 0

      poolSnapshot :: StabilityPoolSnapshot
      poolSnapshot =
        StabilityPoolSnapshot p (spTruncate (d |-| fund)) s snapshotEpoch snapshotScale

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      poolRef :: V2.TxOutRef
      poolInput :: V2.TxOut
      (poolRef, poolInput) =
        Helpers.findUniqueInputWithTokenRef stabilityPoolToken info

      poolValue :: Ledger.Value
      poolValue = V2.txOutValue poolInput

{-# INLINEABLE validateAdjustAccount #-}
validateAdjustAccount ::
  StabilityPoolParams ->
  Ledger.TokenName ->
  StabilityPoolSnapshot ->
  EpochToScaleToSum ->
  Integer ->
  V2.ScriptContext ->
  Bool
validateAdjustAccount
  params@StabilityPoolParams {..}
  asset
  snapshot
  epochToScaleToSum
  depositChange
  ctx =
    traceIfFalse "Invalid input being validated" (ownRef == poolRef)
      && traceIfFalse
        "Wrong signature"
        (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pk))
      && traceIfFalse "The asset does not match" (asset == aAsset)
      && traceIfFalse
        "User cannot have negative balance"
        (fromSPInteger (snapshotD newAccountSnapshot) >= 0)
      && traceIfFalse "Cannot mint any token" (V2.txInfoMint info == mempty)
      && traceIfFalse
        "Expected to consume exactly one Account output"
        (Helpers.isSpendingUnitValue info accountToken)
      && traceIfFalse
        "Stability Pool output does not match"
        ( Helpers.checkOwnOutput
            ctx
            (StabilityPoolDatum asset newPoolSnapshot newEpochToScaleToSum)
            $ poolValue
              <> Ada.lovelaceValueOf (negate reward)
              <> V2.singleton assetSymbol asset depositChange
              <> Ada.lovelaceValueOf accountAdjustmentFeeLovelaces
        )
      && traceIfFalse
        "Account output does not match"
        ( Helpers.checkOwnOutputAdaGeq
            ctx
            (AccountDatum pk asset newAccountSnapshot)
            (Helpers.unitValue accountToken)
        )
      && traceIfFalse
        "Must pay protocol fee correctly"
        (ContractHelpers.payProtocolFeeCorrectly collectorValHash info fee)
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      AccountDatum pk aAsset accountSnapshot =
        Helpers.findInlinedDatumFromOutput
          (Helpers.findUniqueInputWithToken accountToken info)

      reward :: Integer
      adjustedAccount :: StabilityPoolSnapshot
      (adjustedAccount, reward) =
        case adjustAccountHelper
          snapshot
          accountSnapshot
          ( accumulatedEpochToScaleToSum
              params
              asset
              epochToScaleToSum
              info
          ) of
          (adjustedSnap, Just rew) -> (adjustedSnap, rew)
          (_, Nothing) -> traceError "EpochToScaleToSum map missing item"

      protocolFeePercentage :: OnChainDecimal
      protocolFeePercentage = getProtocolFeePercentage govNFT info

      fee :: Integer
      fee = protocolFee protocolFeePercentage reward 0

      newEpochToScaleToSum :: EpochToScaleToSum
      newEpochToScaleToSum =
        AssocMap.insert
          (snapshotEpoch snapshot, snapshotScale snapshot)
          (snapshotS newPoolSnapshot)
          epochToScaleToSum

      newDeposit :: SPInteger
      newDeposit = spTruncate (snapshotD snapshot |+| toSPInteger depositChange)

      newPoolSnapshot :: StabilityPoolSnapshot
      newPoolSnapshot =
        snapshot
          { snapshotD = newDeposit,
            snapshotS =
              snapshotS snapshot
                |+| ( (toSPInteger accountAdjustmentFeeLovelaces |*| snapshotP snapshot)
                        |/| newDeposit
                    )
          }

      newAccountSnapshot :: StabilityPoolSnapshot
      newAccountSnapshot =
        adjustedAccount
          { snapshotD = snapshotD adjustedAccount |+| toSPInteger depositChange
          }

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      poolRef :: V2.TxOutRef
      poolInput :: V2.TxOut
      (poolRef, poolInput) =
        Helpers.findUniqueInputWithTokenRef stabilityPoolToken info

      poolValue :: Ledger.Value
      poolValue = V2.txOutValue poolInput

{-# INLINEABLE accumulatedEpochToScaleToSum #-}
accumulatedEpochToScaleToSum ::
  StabilityPoolParams ->
  Value.TokenName ->
  EpochToScaleToSum ->
  V2.TxInfo ->
  EpochToScaleToSum
accumulatedEpochToScaleToSum
  StabilityPoolParams {snapshotEpochToScaleToSumToken}
  asset
  epochToScaleToSum
  info =
    if all (\a -> snd a == asset) snapshotEpochToScaleToSumInputDatums
      then
        let refInputsAllItems =
              mconcat
                ( map
                    (AssocMap.toList . fst)
                    snapshotEpochToScaleToSumInputDatums
                )
         in AssocMap.fromList
              (refInputsAllItems ++ AssocMap.toList epochToScaleToSum)
      else traceError "EpochToScaleToSum ref inputs have wrong asset"
    where
      snapshotEpochToScaleToSumInputDatums ::
        [(EpochToScaleToSum, Value.TokenName)]
      snapshotEpochToScaleToSumInputDatums =
        map
          extractSnapshotAndAssess
          (filter predicate (V2.txInfoReferenceInputs info))
        where
          predicate =
            Helpers.isAuthOutput snapshotEpochToScaleToSumToken
              . V2.txInInfoResolved
          extractSnapshotAndAssess inp =
            let SnapshotEpochToScaleToSumDatum {sessSnapshot, sessAsset} =
                  Helpers.findInlinedDatumFromOutput (V2.txInInfoResolved inp)
             in (sessSnapshot, sessAsset)

{-# INLINEABLE validateCreateAccount #-}
validateCreateAccount ::
  StabilityPoolParams ->
  Ledger.TokenName ->
  StabilityPoolSnapshot ->
  EpochToScaleToSum ->
  Ledger.PaymentPubKeyHash ->
  Integer ->
  V2.ScriptContext ->
  Bool
validateCreateAccount
  StabilityPoolParams {..}
  asset
  snapshot
  epochToScaleToSum
  pk
  amt
  ctx =
    traceIfFalse "Invalid input being validated" (ownRef == poolRef)
      && traceIfFalse "User cannnot have negative balance" (amt >= 0)
      && traceIfFalse
        "Expected to consume exactly one Stability Pool output"
        (Helpers.isSpendingUnitValue info stabilityPoolToken)
      && traceIfFalse
        "Wrong signature"
        (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pk))
      && traceIfFalse
        "Must mint an account token"
        (V2.txInfoMint info == Helpers.unitValue accountToken)
      && traceIfFalse
        "Stability Pool output does not match"
        ( Helpers.checkOwnOutput
            ctx
            (StabilityPoolDatum asset newPoolSnapshot newEpochToScaleToSum)
            ( poolValue
                <> V2.singleton assetSymbol asset amt
                <> Ada.lovelaceValueOf accountCreateFeeLovelaces
            )
        )
      && traceIfFalse
        "Account output does not match"
        ( Helpers.checkOwnOutputAdaGeq
            ctx
            (AccountDatum pk asset accountSnapshot)
            (Helpers.unitValue accountToken)
        )
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      newEpochToScaleToSum :: EpochToScaleToSum
      newEpochToScaleToSum =
        AssocMap.insert
          (snapshotEpoch snapshot, snapshotScale snapshot)
          (snapshotS newPoolSnapshot)
          epochToScaleToSum

      newDeposit :: SPInteger
      newDeposit = snapshotD snapshot |+| toSPInteger amt

      newPoolSnapshot :: StabilityPoolSnapshot
      newPoolSnapshot =
        snapshot
          { snapshotD = newDeposit,
            snapshotS =
              snapshotS snapshot
                |+| ( (toSPInteger accountCreateFeeLovelaces |*| snapshotP snapshot)
                        |/| newDeposit
                    )
          }

      accountSnapshot :: StabilityPoolSnapshot
      accountSnapshot =
        snapshot
          { snapshotD = toSPInteger amt
          }

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      poolRef :: V2.TxOutRef
      poolInput :: V2.TxOut
      (poolRef, poolInput) =
        Helpers.findUniqueInputWithTokenRef stabilityPoolToken info

      poolValue :: Ledger.Value
      poolValue = V2.txOutValue poolInput

stabilityPoolAddress :: StabilityPoolParams -> Ledger.Address
stabilityPoolAddress = Address.mkValidatorAddress . stabilityPoolValidator
