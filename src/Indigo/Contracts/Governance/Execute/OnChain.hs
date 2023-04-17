-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.Governance.Execute.OnChain
  ( validateExecute,
    executeAddress,
    executeValidator,
    executeScriptCTL,
    untypedExecuteHash,
  )
where

import Indigo.Contracts.CDP.Common (IAsset (iaMinRatio, iaName, iaPrice))
import Indigo.Contracts.CDP.Common qualified as CDP
import Indigo.Contracts.CDP.OnChain qualified as CDP
import Indigo.Contracts.Governance.Execute.Common
  ( ExecuteParams
      ( ExecuteParams,
        cdpValHash,
        govNFT,
        iAssetToken,
        sPoolValHash,
        stabilityPoolToken,
        upgradeToken,
        versionRecordToken,
        versionRegistryValHash
      ),
    ExecuteRedeemer (Execute),
    Upgrade (Upgrade, uContent, uEndTime, uPassedTime, uProtocolVersion),
  )
import Indigo.Contracts.Governance.Gov.Common qualified as Gov
import Indigo.Contracts.Governance.VersionRegistry.Common
  ( VersionRecord (VersionRecord),
  )
import Indigo.Contracts.StabilityPool.Common
  ( StabilityDatum (epochToScaleToSum, spIAsset, spSnapshot),
    initEpochToScaleToSumMap,
    initSPSnapshot,
  )
import Indigo.Contracts.StabilityPool.Common qualified as StabilityPool
import Indigo.Utils.Helpers qualified as Helpers
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
import PlutusTx.Prelude

{-# INLINEABLE validateExecute #-}
validateExecute ::
  ExecuteParams ->
  Upgrade ->
  ExecuteRedeemer ->
  V2.ScriptContext ->
  Bool
validateExecute
  param@ExecuteParams {govNFT, upgradeToken}
  Upgrade {uContent, uEndTime, uPassedTime, uProtocolVersion}
  Execute
  ctx =
    traceIfFalse
      "Mixed protocol versions"
      (Gov.currentVersion govDatumInput == uProtocolVersion)
      && traceIfFalse
        "Can only consume one upgrade token"
        (Helpers.hasUniqueInputWithToken upgradeToken info)
      && traceIfFalse
        "Must execute after effective delay"
        ( Interval.ivFrom validityRange
            > Interval.lowerBound
              (uPassedTime + Gov.effectiveDelay protocolParams)
        )
      && ( if Interval.upperBound
             (uEndTime + Gov.expirationPeriod protocolParams)
             >= Interval.ivTo validityRange -- Passed
             then
               traceIfFalse
                 "Fail to execute upgrade in executing period"
                 ( validateExecuteSuccess
                     param
                     govValHash
                     consumesGov
                     govDatumInput
                     uContent
                     ctx
                 )
             else
               traceIfFalse
                 "Transaction must not consume Gov for failed upgrade"
                 (not consumesGov)
                 && traceIfFalse
                   "Transaction must burn exactly 1 upgrade token"
                   (V2.txInfoMint info == inv (Helpers.unitValue upgradeToken))
                 && traceIfFalse
                   "Must validate after expiration period for failed upgrade"
                   ( Interval.ivFrom validityRange
                       > Interval.lowerBound
                         (uEndTime + Gov.expirationPeriod protocolParams)
                   )
         )
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      filterGovNft :: [V2.TxInInfo] -> [V2.TxOut]
      filterGovNft =
        Utils.filterMap (Helpers.isAuthOutput govNFT) V2.txInInfoResolved

      mGovInput :: Maybe V2.TxOut
      mGovInput = case filterGovNft $ V2.txInfoInputs info of
        [out] -> Just out
        _ -> Nothing

      mGovRefInput :: Maybe V2.TxOut
      mGovRefInput = case filterGovNft $ V2.txInfoReferenceInputs info of
        [refOut] -> Just refOut
        _ -> Nothing

      consumesGov :: Bool
      consumesGov = case mGovInput of
        Just _ -> True
        Nothing -> False

      govInput :: Maybe V2.TxOut
      govInput = if consumesGov then mGovInput else mGovRefInput

      govValHash :: V2.ValidatorHash
      govValHash = case govInput >>= Ledger.toValidatorHash . V2.txOutAddress of
        Nothing -> traceError "Cannot find Gov script"
        Just x -> x

      govDatumInput :: Gov.GovDatum
      govDatumInput@Gov.Gov {Gov.protocolParams} =
        case govInput of
          Just inp -> Helpers.findInlinedDatumFromOutput inp
          Nothing -> traceError "Expected Gov NFT as input or ref. input"

      validityRange :: Ledger.POSIXTimeRange
      validityRange = V2.txInfoValidRange info

{-# INLINEABLE validateExecuteSuccess #-}
validateExecuteSuccess ::
  ExecuteParams ->
  V2.ValidatorHash ->
  Bool ->
  Gov.GovDatum ->
  Gov.ProposalContent ->
  V2.ScriptContext ->
  Bool
validateExecuteSuccess
  param@ExecuteParams
    { iAssetToken,
      govNFT,
      stabilityPoolToken,
      versionRecordToken,
      upgradeToken
    }
  govValHash
  consumesGov
  govDatumInput
  proposal
  ctx =
    traceIfFalse "The datum of governance must update properly" properGovUpdate
      && traceIfFalse
        "The transaction must mint properly"
        (V2.txInfoMint info == expectedMintTx)
      && traceIfFalse
        "Unauthorized to spend IAsset output"
        unauthorizedSpendIAssetOutput
      && validateExecuteSuccessExtra param proposal info
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      properGovUpdate :: Bool
      properGovUpdate = case proposal of
        Gov.ModifyProtocolParams pParam ->
          Helpers.checkOutputFromOtherScriptsAdaGeq
            info
            govValHash
            (govDatumInput {Gov.protocolParams = pParam})
            (Helpers.unitValue govNFT)
        Gov.UpgradeProtocol {} ->
          Helpers.checkOutputFromOtherScriptsAdaGeq
            info
            govValHash
            ( govDatumInput
                { Gov.currentVersion = Gov.currentVersion govDatumInput + 1
                }
            )
            (Helpers.unitValue govNFT)
        _ -> not consumesGov

      expectedMintTx :: Ledger.Value
      expectedMintTx = case proposal of
        Gov.ProposeAsset {} ->
          Helpers.unitValue stabilityPoolToken
            <> Helpers.unitValue iAssetToken
            <> inv (Helpers.unitValue upgradeToken)
        Gov.UpgradeProtocol {} ->
          Helpers.unitValue versionRecordToken
            <> inv (Helpers.unitValue upgradeToken)
        _ -> inv (Helpers.unitValue upgradeToken)

      unauthorizedSpendIAssetOutput :: Bool
      unauthorizedSpendIAssetOutput = case proposal of
        Gov.MigrateAsset {} -> True
        _ -> Value.assetClassValueOf (Contexts.valueSpent info) iAssetToken == 0

{-# INLINEABLE validateExecuteSuccessExtra #-}
validateExecuteSuccessExtra ::
  ExecuteParams ->
  Gov.ProposalContent ->
  V2.TxInfo ->
  Bool
validateExecuteSuccessExtra
  ExecuteParams {cdpValHash, iAssetToken, sPoolValHash, stabilityPoolToken}
  (Gov.ProposeAsset iaName iaRatio iaOracle)
  info =
    traceIfFalse
      "StabilityPool output is incorrect"
      ( Helpers.checkOutputFromOtherScriptsAdaGeq
          info
          sPoolValHash
          ( StabilityPool.StabilityPoolDatum
              { spIAsset = iaName,
                spSnapshot = initSPSnapshot,
                epochToScaleToSum = initEpochToScaleToSumMap
              }
          )
          (Helpers.unitValue stabilityPoolToken)
      )
      && traceIfFalse
        "iAsset output is incorrect"
        ( Helpers.checkOutputFromOtherScriptsAdaGeq
            info
            cdpValHash
            ( CDP.IAssetDatum $
                CDP.IAsset
                  { iaName,
                    iaMinRatio = iaRatio,
                    iaPrice = Right iaOracle
                  }
            )
            (Helpers.unitValue iAssetToken)
        )
validateExecuteSuccessExtra
  ExecuteParams {cdpValHash, iAssetToken}
  (Gov.MigrateAsset iaName iaRatio iaOracle)
  info =
    traceIfFalse
      "iAsset output is incorrect"
      ( Helpers.checkOutputFromOtherScriptsAdaGeq
          info
          cdpValHash
          ( CDP.IAssetDatum $
              CDP.IAsset {iaName, iaMinRatio = iaRatio, iaPrice = iaOracle}
          )
          (Helpers.unitValue iAssetToken)
      )
      && traceIfFalse "Spend invalid iAsset output" (assetName == iaName)
      && case oracle of
        Left _ -> traceError "Cannot migrate a delisted iAsset"
        _ -> True
      && traceIfFalse
        "Migrating non-upgraded IAsset"
        ( Ledger.toValidatorHash (V2.txOutAddress iAssetInput)
            == Just cdpValHash
        )
    where
      (assetName, _, oracle) = CDP.getIAssetInfo iAssetInput

      iAssetInput :: V2.TxOut
      iAssetInput = Helpers.findUniqueInputWithToken iAssetToken info
validateExecuteSuccessExtra
  ExecuteParams {versionRecordToken, versionRegistryValHash}
  (Gov.UpgradeProtocol Gov.UpgradePaths {Gov.uId, Gov.uPaths})
  info =
    traceIfFalse
      "versionRecord output is incorrect"
      ( Helpers.checkOutputFromOtherScriptsAdaGeq
          info
          versionRegistryValHash
          (VersionRecord uId $ Gov.upgradeSymbol <$> uPaths)
          (Helpers.unitValue versionRecordToken)
      )
validateExecuteSuccessExtra ExecuteParams {} Gov.ModifyProtocolParams {} _ =
  True
validateExecuteSuccessExtra ExecuteParams {} (Gov.TextProposal _) _ = True

executeValidator :: ExecuteParams -> V2.Validator
executeValidator param =
  V2.mkValidatorScript
    (compiledValidateExecute `PlutusTx.applyCode` PlutusTx.liftCode param)

compiledValidateExecute ::
  PlutusTx.CompiledCode (ExecuteParams -> UntypedValidator)
compiledValidateExecute =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||mkUntypedValidator . validateExecute||])

compiledUntypedValidateExecute ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidateExecute =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          mkUntypedValidator
            . validateExecute
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedExecuteValidator :: BuiltinData -> V2.Validator
untypedExecuteValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidateExecute
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedExecuteHash :: BuiltinData -> V2.ValidatorHash
untypedExecuteHash = Scripts.validatorHash . untypedExecuteValidator

-- serialised for use in CTL
executeScriptCTL :: V2.Script
executeScriptCTL = V2.fromCompiledCode compiledUntypedValidateExecute

executeAddress :: ExecuteParams -> Ledger.Address
executeAddress = Address.mkValidatorAddress . executeValidator
