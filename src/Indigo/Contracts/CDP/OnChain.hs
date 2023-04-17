-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.CDP.OnChain
  ( validateCDPScript,
    validateCDPCreatorScript,
    cdpScriptAddress,
    cdpScriptValidator,
    mkIAssetPolicy,
    untypedCDPScriptHash,
    cdpCreatorScriptAddress,
    cdpCreatorScriptValidator,
    untypedCDPCreatorValidatorHash,
    iAssetPolicy,
    getIAssetInfo,
    getIAssetPrice,
    iAssetSymbol,
    iAssetPolicyHash,
    iAssetPolicyScriptCTL,
    cdpCreatorScriptCTL,
    cdpScriptCTL,
    untypedIAssetPolicySymbol,
  )
where

import Indigo.Contracts.CDP.Common
import Indigo.Contracts.Governance.VersionRegistry.Common
  ( validateUpgradeVersion,
  )
import Indigo.Contracts.Helpers (getProtocolFeePercentage)
import Indigo.Contracts.Helpers qualified as ContractHelpers
import Indigo.Contracts.Oracle.Common
import Indigo.Data.Decimal (OnChainDecimal (..))
import Indigo.Utils.Helpers qualified as Helpers
import Indigo.Utils.Spooky qualified as Spooky
import Indigo.Utils.Spooky.Helpers qualified as Spooky.Helpers
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as V2
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Address qualified as Address
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies
  ( UntypedMintingPolicy,
  )
import Plutus.Script.Utils.V2.Typed.Scripts.Validators
  ( UntypedValidator,
    mkUntypedValidator,
  )
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.Prelude hiding (fromInteger)

{-# INLINEABLE getIAssetPrice #-}
getIAssetPrice :: OracleAssetNFT -> V2.TxInfo -> (Bool, OnChainDecimal)
getIAssetPrice (MkOracleAssetNFT oAssetNFT) info = (isExpired, odPrice)
  where
    oracleInput :: V2.TxOut
    oracleInput = Helpers.findUniqueReferenceInputWithToken oAssetNFT info

    MkOracleDatum {odPrice, odExpiration} =
      Helpers.findInlinedDatumFromOutput oracleInput

    isExpired :: Bool
    isExpired =
      Ledger.ivTo (V2.txInfoValidRange info) >= Ledger.upperBound odExpiration

-- | This gets TxOut as a parameter since it can be also reference input.
-- The caller decides what type of input it is.
{-# INLINEABLE getIAssetInfo #-}
getIAssetInfo ::
  V2.TxOut ->
  (Value.TokenName, OnChainDecimal, Either OnChainDecimal OracleAssetNFT)
getIAssetInfo iAssetInput = (iaName, iaMinRatio, iaPrice)
  where
    IAssetDatum iAsset = Helpers.findInlinedDatumFromOutput iAssetInput

    IAsset {iaName, iaMinRatio, iaPrice} = iAsset

{-# INLINEABLE collateralAmount #-}
collateralAmount :: V2.TxOut -> Integer
collateralAmount = Ada.getLovelace . Ada.fromValue . V2.txOutValue

{-# INLINEABLE validateCreate #-}
validateCreate ::
  CDPCreatorScriptParams ->
  Ledger.PaymentPubKeyHash ->
  Integer ->
  Integer ->
  V2.ScriptContext ->
  Bool
validateCreate CDPCreatorScriptParams {..} pkh minted collateral ctx =
  traceIfFalse "Mixed versions" (iAssetValHash == cdpScriptHash)
    && traceIfFalse
      "The transaction is not signed by cdp owner"
      (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pkh))
    && traceIfFalse
      "CDPCreatorNFT missing"
      ( Helpers.hasUnitValue
          (Helpers.valueWithin $ Helpers.findOwnInput' ctx)
          cdpCreatorNft
      )
    && traceIfFalse
      "CDPCreator output does not match"
      ( Helpers.checkOwnOutputAdaGeq @CDPCreatorDatum ctx () $
          Helpers.unitValue cdpCreatorNft
      )
    && traceIfFalse
      "Minted value is invalid"
      ( V2.txInfoMint info
          == Value.singleton cdpAssetCs iaName minted
            <> Helpers.unitValue cdpAuthTk
      )
    && traceIfFalse
      "CDP output is invalid"
      ( Helpers.checkOutputFromOtherScriptsWithStakingCredential
          info
          cdpScriptHash
          cdpDatum
          cdpValue
      )
    && traceIfFalse
      "Undercollaterized CDP"
      (overCollaterized collateral minted iAssetPrice iaMinRatio)
    && traceIfFalse "Negative minted amount" (minted >= 0)
    && traceIfFalse
      "Must provide more than minCollateralInLovelace"
      (collateral >= minCollateralInLovelace)
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    iAssetInput :: V2.TxOut
    iAssetInput = Helpers.findUniqueReferenceInputWithToken iAssetAuthTk info

    iAssetValHash :: Ledger.ValidatorHash
    iAssetValHash = case Ledger.toValidatorHash (V2.txOutAddress iAssetInput) of
      Just valHash -> valHash
      Nothing -> traceError "Could not get validator hash"

    iaName :: Value.TokenName
    iaMinRatio :: OnChainDecimal
    iaPrice :: Either OnChainDecimal OracleAssetNFT
    (iaName, iaMinRatio, iaPrice) = getIAssetInfo iAssetInput

    cdpValue :: Ledger.Value
    cdpValue = Ada.lovelaceValueOf collateral <> Helpers.unitValue cdpAuthTk

    cdpDatum :: CDPDatum
    cdpDatum = CDP (Just pkh) iaName minted

    iAssetPrice :: OnChainDecimal
    iAssetPrice = case iaPrice of
      Left _ -> traceError "Cannot open a CDP of a delisted asset"
      Right oAssetNFT -> getPriceFromOracle $ getIAssetPrice oAssetNFT info

    getPriceFromOracle :: (Bool, OnChainDecimal) -> OnChainDecimal
    getPriceFromOracle (expired, price) =
      if expired
        then traceError "Oracle cannot be expired"
        else price

{-# INLINEABLE validateAdjustCDP #-}
validateAdjustCDP ::
  CDPScriptParams ->
  Maybe Ledger.PaymentPubKeyHash ->
  Value.TokenName ->
  Integer ->
  V2.ScriptContext ->
  Bool
validateAdjustCDP _ Nothing _ _ _ = traceError "Cannot adjust frozen CDP"
validateAdjustCDP CDPScriptParams {..} (Just pk) asset ma ctx =
  traceIfFalse "Invalid input being validated" (ownRef == cdpRef)
    && traceIfFalse "Mixed versions" (iAssetValHash == Contexts.ownHash ctx)
    && traceIfFalse
      "The transaction is not signed by cdp owner"
      (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pk))
    && traceIfFalse
      "Minted/Burnt value is invalid"
      ( V2.txInfoMint info
          == Value.singleton cdpAssetSymbol asset mintedAmount
      )
    && traceIfFalse "Incorrect iAsset input" (iaName == asset)
    && traceIfFalse
      "CDP output is incorrect"
      ( Helpers.checkOwnOutput @CDPDatum
          ctx
          (CDP (Just pk) asset (ma + mintedAmount))
          ( Ada.lovelaceValueOf (collateralAmount cdpOutput)
              <> Helpers.unitValue cdpAuthToken
          )
      )
    -- We only need to check the collateralization of the CDP when
    -- withdrawing collateral or minting iAssets.
    && ( (mintedAmount <= 0 && inputCollateral <= outputCollateral)
           || traceIfFalse
             "Undercollaterized CDP"
             ( overCollaterized
                 (collateralAmount cdpOutput)
                 (ma + mintedAmount)
                 iAssetPrice
                 iaMinRatio
             )
       )
    && traceIfFalse "Negative minted amount" (ma + mintedAmount >= 0)
    && traceIfFalse
      "Cannot mint delisted iAsset"
      (isRight iaPrice || mintedAmount <= 0)
    && traceIfFalse
      "Must pay protocol fee correctly"
      (ContractHelpers.payProtocolFeeCorrectly collectorValHash info fee)
    && traceIfFalse
      "Must provide more than minCollateralInLovelace"
      (outputCollateral >= minCollateralInLovelace)
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    ownRef :: V2.TxOutRef
    ownRef = case V2.scriptContextPurpose ctx of
      Contexts.Spending ref -> ref
      _ -> traceError "Must spend UTxO"

    cdpRef :: V2.TxOutRef
    cdpInput :: V2.TxOut
    (cdpRef, cdpInput) = Helpers.findUniqueInputWithTokenRef cdpAuthToken info

    cdpOutput :: V2.TxOut
    cdpOutput = Helpers.findUniqueOutputFromCurrentScript cdpAuthToken ctx

    outputCollateral :: Integer
    outputCollateral = collateralAmount cdpOutput

    inputCollateral :: Integer
    inputCollateral = collateralAmount cdpInput

    iAssetInput :: V2.TxOut
    iAssetInput = Helpers.findUniqueReferenceInputWithToken iAssetAuthToken info

    iAssetValHash :: Ledger.ValidatorHash
    iAssetValHash = case Ledger.toValidatorHash (V2.txOutAddress iAssetInput) of
      Just valHash -> valHash
      Nothing -> traceError "Could not get validator hash"

    iaName :: Value.TokenName
    iaMinRatio :: OnChainDecimal
    iaPrice :: Either OnChainDecimal OracleAssetNFT
    (iaName, iaMinRatio, iaPrice) = getIAssetInfo iAssetInput

    iAssetPrice :: OnChainDecimal
    iAssetPrice = case iaPrice of
      -- If delisted, pass the lastPrice.
      Left lastPrice -> lastPrice
      -- Additionally check if expired.
      Right oAssetNFT -> getPriceFromOracle $ getIAssetPrice oAssetNFT info
    getPriceFromOracle :: (Bool, OnChainDecimal) -> OnChainDecimal
    getPriceFromOracle (expired, price) =
      if expired
        then traceError "Oracle cannot be expired"
        else price

    mintedAmount :: Integer
    mintedAmount =
      Value.assetClassValueOf
        (V2.txInfoMint info)
        (Value.assetClass cdpAssetSymbol asset)

    protocolFeePercentage :: OnChainDecimal
    protocolFeePercentage = getProtocolFeePercentage govNFT info

    fee :: Integer
    fee = protocolFee protocolFeePercentage inputCollateral outputCollateral

{-# INLINEABLE validateCloseCDP #-}
validateCloseCDP ::
  CDPScriptParams ->
  Maybe Ledger.PaymentPubKeyHash ->
  Value.TokenName ->
  Integer ->
  V2.ScriptContext ->
  Bool
validateCloseCDP _ Nothing _ _ _ = traceError "Cannot close frozen CDP"
validateCloseCDP CDPScriptParams {..} (Just pk) asset ma ctx =
  traceIfFalse "Invalid input being validated" (ownRef == cdpRef)
    && traceIfFalse "Mixed versions" (iAssetValHash == Contexts.ownHash ctx)
    && traceIfFalse
      "The transaction is not signed by cdp owner"
      (Contexts.txSignedBy info (Ledger.unPaymentPubKeyHash pk))
    && traceIfFalse
      "Burned value is invalid"
      ( V2.txInfoMint info
          == negate (Helpers.unitValue cdpAuthToken)
            <> Value.singleton cdpAssetSymbol asset (negate ma)
      )
    && traceIfFalse
      "Must pay protocol fee correctly"
      (ContractHelpers.payProtocolFeeCorrectly collectorValHash info fee)
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    ownRef :: V2.TxOutRef
    ownRef = case V2.scriptContextPurpose ctx of
      Contexts.Spending ref -> ref
      _ -> traceError "Must spend UTxO"

    cdpRef :: V2.TxOutRef
    cdpInput :: V2.TxOut
    (cdpRef, cdpInput) = Helpers.findUniqueInputWithTokenRef cdpAuthToken info

    protocolFeePercentage :: OnChainDecimal
    protocolFeePercentage = getProtocolFeePercentage govNFT info

    fee :: Integer
    fee = protocolFee protocolFeePercentage (collateralAmount cdpInput) zero

    iAssetInput :: V2.TxOut
    iAssetInput = Helpers.findUniqueReferenceInputWithToken iAssetAuthToken info

    iAssetValHash :: Ledger.ValidatorHash
    iAssetValHash = case Ledger.toValidatorHash (V2.txOutAddress iAssetInput) of
      Just valHash -> valHash
      Nothing -> traceError "Could not get validator hash"

{-# INLINEABLE validateLiquidateCDP #-}
validateLiquidateCDP ::
  CDPScriptParams ->
  Maybe Ledger.PaymentPubKeyHash ->
  Value.TokenName ->
  Integer ->
  V2.ScriptContext ->
  Bool
validateLiquidateCDP
  CDPScriptParams
    { stabilityPoolAuthToken,
      cdpAuthToken,
      cdpAssetSymbol,
      spValHash
    }
  owner
  asset
  ma
  ctx =
    traceIfFalse "Invalid input being validated" (ownRef == cdpRef)
      && traceIfFalse "CDP needs to be frozen" (isNothing owner)
      && traceIfFalse "More than one SP input" (null $ tail spInputs)
      && traceIfFalse
        "Wrong SP token"
        (Helpers.isAuthOutput stabilityPoolAuthToken spInput)
      && ( if isPartialLiquidation
             then
               traceIfFalse
                 "CDP output is incorrect"
                 ( Helpers.checkOwnOutput @CDPDatum
                     ctx
                     (CDP owner asset (ma - burntIAsset))
                     correctCdpContinuingValue
                 )
                 && traceIfFalse
                   "Stability pool output value is incorrect"
                   ( spOutputValue
                       == spInputValue
                         <> partialLiqSeizedValue
                         <> burntIAssetVal
                   )
                 && traceIfFalse
                   "Expected burn all iAssets in SP"
                   ( txMint
                       == negate
                         ( Helpers.valueOfAssetCls'
                             spInputValue
                             cdpAssetSymbol
                             asset
                         )
                   )
             else
               traceIfFalse
                 "Expected burn all cdp auth tokens in cdp input and all minted iAssets"
                 ( txMint
                     == negate
                       ( Helpers.valueOfAssetCls
                           cdpInputValue
                           cdpAuthToken
                       )
                       <> negate
                         ( Value.singleton
                             cdpAssetSymbol
                             asset
                             ma
                         )
                 )
                 && traceIfFalse
                   "Stability pool output value is incorrect"
                   ( spOutputValue
                       == spInputValue
                         <> Value.adaOnlyValue cdpInputValue
                         <> burntIAssetVal
                   )
         )
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      cdpRef :: V2.TxOutRef
      cdpInput :: V2.TxOut
      (cdpRef, cdpInput) =
        Helpers.findUniqueInputWithPositiveAmtOfTokensRef cdpAuthToken info

      cdpInputValue :: Ledger.Value
      cdpInputValue = V2.txOutValue cdpInput

      spInputs :: [V2.TxOut]
      spInputs = Helpers.findAllInputsFromScript spValHash info

      spInput :: V2.TxOut
      [spInput] = spInputs

      spInputValue :: Ledger.Value
      spInputValue = V2.txOutValue spInput

      spOutput :: V2.TxOut
      [spOutput] = Helpers.findAllOutputsToAddress spValHash info

      spOutputValue :: Ledger.Value
      spOutputValue = V2.txOutValue spOutput

      txMint :: Ledger.Value
      txMint = V2.txInfoMint info

      mintedIAsset :: Integer
      mintedIAsset = Value.valueOf txMint cdpAssetSymbol asset

      burntIAsset :: Integer
      burntIAsset = negate mintedIAsset

      burntIAssetVal :: V2.Value
      burntIAssetVal = Value.singleton cdpAssetSymbol asset mintedIAsset

      isPartialLiquidation :: Bool
      isPartialLiquidation = burntIAsset < ma

      partialLiqSeizedValue :: V2.Value
      partialLiqSeizedValue =
        Ada.lovelaceValueOf $
          (Helpers.lovelacesAmount cdpInputValue * burntIAsset) `divide` ma

      correctCdpContinuingValue :: V2.Value
      correctCdpContinuingValue = cdpInputValue <> negate partialLiqSeizedValue

{-# INLINEABLE validateUpgradeIAsset #-}
validateUpgradeIAsset :: Value.AssetClass -> V2.ScriptContext -> Bool
validateUpgradeIAsset uToken ctx =
  traceIfFalse
    "Spend exactly 1 Upgrade Token"
    (Helpers.isSpendingUnitValue info uToken)
    && traceIfFalse
      "Burn the upgrade token and nothing else"
      (V2.txInfoMint info == inv (Helpers.unitValue uToken))
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

{-# INLINEABLE validateFreezeCDP #-}
validateFreezeCDP ::
  CDPScriptParams ->
  Maybe Ledger.PaymentPubKeyHash ->
  Ledger.TokenName ->
  Integer ->
  Contexts.ScriptContext ->
  Bool
validateFreezeCDP
  CDPScriptParams {cdpAuthToken, iAssetAuthToken}
  owner
  asset
  mintedAmount
  ctx =
    traceIfFalse "Invalid input being validated" (ownRef == cdpRef)
      && traceIfFalse "Mixed versions" (iAssetValHash == Contexts.ownHash ctx)
      && traceIfFalse "CDP is already frozen" (isJust owner)
      && traceIfFalse "Invalid iAsset input" (iaName == asset)
      && traceIfFalse "Can not mint any token" (V2.txInfoMint info == mempty)
      && traceIfFalse
        "CDP still overcollaterized"
        ( not $
            overCollaterized
              (collateralAmount cdpInput)
              mintedAmount
              iAssetPrice
              iaMinRatio
        )
      && traceIfFalse
        "CDP output is incorrect"
        ( Helpers.checkOwnOutputNoStaking @CDPDatum
            ctx
            CDP
              { cdpOwner = Nothing,
                cdpIAsset = asset,
                cdpMintedAmount = mintedAmount
              }
            (V2.txOutValue cdpInput)
        )
    where
      info :: V2.TxInfo
      info = V2.scriptContextTxInfo ctx

      iAssetInput :: V2.TxOut
      iAssetInput =
        Helpers.findUniqueReferenceInputWithToken iAssetAuthToken info

      iAssetValHash :: Ledger.ValidatorHash
      iAssetValHash =
        case Ledger.toValidatorHash (V2.txOutAddress iAssetInput) of
          Just valHash -> valHash
          Nothing -> traceError "Could not get validator hash"

      iaName :: Value.TokenName
      iaMinRatio :: OnChainDecimal
      iaPrice :: Either OnChainDecimal OracleAssetNFT
      (iaName, iaMinRatio, iaPrice) = getIAssetInfo iAssetInput

      ownRef :: V2.TxOutRef
      ownRef = case V2.scriptContextPurpose ctx of
        Contexts.Spending ref -> ref
        _ -> traceError "Must spend UTxO"

      iAssetPrice :: OnChainDecimal
      iAssetPrice = case iaPrice of
        -- iAsset is delisted, use the last oracle price.
        Left p -> p
        -- Oracle is active, we don't care if the Oracle is expired.
        Right oAssetNFT -> snd $ getIAssetPrice oAssetNFT info

      cdpRef :: V2.TxOutRef
      cdpInput :: V2.TxOut
      (cdpRef, cdpInput) = Helpers.findUniqueInputWithTokenRef cdpAuthToken info

{-# INLINEABLE validateMergeCDPs #-}
validateMergeCDPs ::
  CDPScriptParams ->
  V2.TokenName ->
  Contexts.ScriptContext ->
  Bool
validateMergeCDPs CDPScriptParams {cdpAuthToken} asset ctx =
  traceIfFalse "Invalid input being validated" (length currentCDPInput == 1)
    && traceIfFalse
      "Needs at least 1 other CDP input"
      (not (null otherCDPInputs))
    && traceIfFalse
      "All other CDP inputs are spent with MergeAuxiliary redeemer"
      ( all
          ( \input ->
              Helpers.usesSpendRedeemer
                info
                (V2.txInInfoOutRef input)
                (MergeAuxiliary ownRef)
                && V2.txOutAddress (V2.txInInfoResolved input)
                  == Ledger.scriptHashAddress ownValHash
          )
          otherCDPInputs
      )
    && traceIfFalse
      "All CDP inputs are frozen CDP positions with the same asset"
      ( all
          (\(owner, iAsset, _) -> isNothing owner && asset == iAsset)
          cdpInputDatums
      )
    && traceIfFalse
      "A single output has total value from all input CDPs (inc. auth tokens)"
      (V2.txOutValue cdpOutput == totalCDPValue)
    && traceIfFalse
      "Nothing minted/burnt"
      (V2.txInfoMint info == mempty)
    && traceIfFalse
      "The merged CDP has correct datum"
      ( Helpers.hasExpectedInlinedDatum
          cdpOutput
          ( CDP
              { cdpOwner = Nothing,
                cdpIAsset = asset,
                cdpMintedAmount = totalIAssetAmount
              }
          )
      )
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    ownRef :: V2.TxOutRef
    ownRef = case V2.scriptContextPurpose ctx of
      Contexts.Spending ref -> ref
      _ -> traceError "Must spend UTxO"

    ownValHash :: V2.ValidatorHash
    ownValHash = Contexts.ownHash ctx

    totalCDPValue :: Ledger.Value
    totalCDPValue = foldMap V2.txOutValue allCDPInputs

    totalIAssetAmount :: Integer
    totalIAssetAmount =
      foldr (\(_, _, mintedAmount) !acc -> acc + mintedAmount) 0 cdpInputDatums

    cdpInputDatums ::
      [(Maybe Ledger.PaymentPubKeyHash, Value.TokenName, Integer)]
    cdpInputDatums =
      fmap
        ( \out -> case Helpers.findInlinedDatumFromOutput @CDPDatum out of
            CDP {cdpOwner, cdpIAsset, cdpMintedAmount} ->
              (cdpOwner, cdpIAsset, cdpMintedAmount)
            _ -> traceError "Wrong datum at CDP merge input"
        )
        allCDPInputs

    [cdpOutput] =
      filter
        (Helpers.hasPositiveValue cdpAuthToken . V2.txOutValue)
        (Contexts.getContinuingOutputs ctx)

    allCDPInputs :: [V2.TxOut]
    allCDPInputs = V2.txInInfoResolved <$> allCDPInputs'

    allCDPInputs' :: [V2.TxInInfo]
    allCDPInputs' =
      filter
        ( Helpers.hasPositiveValue cdpAuthToken
            . V2.txOutValue
            . V2.txInInfoResolved
        )
        (V2.txInfoInputs info)

    currentCDPInput :: [V2.TxInInfo]
    otherCDPInputs :: [V2.TxInInfo]
    (currentCDPInput, otherCDPInputs) =
      partition (\input -> V2.txInInfoOutRef input == ownRef) allCDPInputs'

{-# INLINEABLE validateMergeAuxiliary #-}
validateMergeAuxiliary :: V2.TxOutRef -> Contexts.ScriptContext -> Bool
validateMergeAuxiliary mainMergeUtxo ctx =
  traceIfFalse
    "The main merge UTXO is using MergeCDPs redeemer"
    (Helpers.usesSpendRedeemer info mainMergeUtxo MergeCDPs)
    && traceIfFalse
      "merge UTXO comes from the same script"
      ( any
          ( \a ->
              V2.txInInfoOutRef a == mainMergeUtxo
                && (V2.txOutAddress . V2.txInInfoResolved) a
                  == Ledger.scriptHashAddress ownValHash
          )
          (V2.txInfoInputs info)
      )
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    ownValHash :: V2.ValidatorHash
    ownValHash = Contexts.ownHash ctx

{-# INLINEABLE validateCDPScript #-}
validateCDPScript ::
  CDPScriptParams ->
  CDPDatum ->
  CDPRedeemer ->
  V2.ScriptContext ->
  Bool
validateCDPScript param CDP {..} AdjustCDP ctx =
  validateAdjustCDP param cdpOwner cdpIAsset cdpMintedAmount ctx
validateCDPScript param CDP {..} CloseCDP ctx =
  validateCloseCDP param cdpOwner cdpIAsset cdpMintedAmount ctx
validateCDPScript param CDP {..} Liquidate ctx =
  validateLiquidateCDP param cdpOwner cdpIAsset cdpMintedAmount ctx
validateCDPScript param IAssetDatum {} UpgradeAsset ctx =
  validateUpgradeIAsset (upgradeToken param) ctx
validateCDPScript CDPScriptParams {versionRecordToken} _ UpgradeVersion ctx =
  validateUpgradeVersion "CDP" ctx versionRecordToken
validateCDPScript param CDP {..} FreezeCDP ctx =
  validateFreezeCDP param cdpOwner cdpIAsset cdpMintedAmount ctx
validateCDPScript param CDP {..} MergeCDPs ctx =
  validateMergeCDPs param cdpIAsset ctx
validateCDPScript _ CDP {} MergeAuxiliary {mainMergeUtxo} ctx =
  validateMergeAuxiliary mainMergeUtxo ctx
validateCDPScript _ _ _ _ = False

{-# INLINEABLE validateCDPCreatorScript #-}
validateCDPCreatorScript ::
  CDPCreatorScriptParams ->
  CDPCreatorDatum ->
  CDPCreatorRedeemer ->
  V2.ScriptContext ->
  Bool
validateCDPCreatorScript param _ (CreateCDP pkh minted collateral) ctx =
  validateCreate param pkh minted collateral ctx
validateCDPCreatorScript
  CDPCreatorScriptParams {versionRecordToken}
  _
  UpgradeCreatorVersion
  ctx =
    validateUpgradeVersion "CDPCreator" ctx versionRecordToken

-- | Minting Policy for minting/burning iAsset.
{-# INLINEABLE mkIAssetPolicy #-}
mkIAssetPolicy :: Spooky.AssetClass -> () -> Spooky.ScriptContext -> Bool
mkIAssetPolicy cdpAuthToken _ ctx =
  traceIfFalse
    "CDPToken missing"
    ( Spooky.Helpers.hasPositiveValue valueIn cdpAuthToken
        || Spooky.Helpers.hasUnitValue valueOut cdpAuthToken
    )
  where
    info = Spooky.scriptContextTxInfo ctx
    valueIn = Spooky.valueSpent info
    valueOut = Spooky.valueProduced info

cdpScriptValidator :: CDPScriptParams -> V2.Validator
cdpScriptValidator param =
  V2.mkValidatorScript
    (compiledValidateCDPScript `PlutusTx.applyCode` PlutusTx.liftCode param)

compiledValidateCDPScript ::
  PlutusTx.CompiledCode (CDPScriptParams -> UntypedValidator)
compiledValidateCDPScript =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||mkUntypedValidator . validateCDPScript||])

compiledUntypedValidateCDPScript ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidateCDPScript =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          mkUntypedValidator
            . validateCDPScript
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedCDPScriptValidator :: BuiltinData -> V2.Validator
untypedCDPScriptValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidateCDPScript
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedCDPScriptHash :: BuiltinData -> V2.ValidatorHash
untypedCDPScriptHash = Scripts.validatorHash . untypedCDPScriptValidator

-- serialised for use in CTL
cdpScriptCTL :: V2.Script
cdpScriptCTL = V2.fromCompiledCode compiledUntypedValidateCDPScript

cdpScriptAddress :: CDPScriptParams -> Ledger.Address
cdpScriptAddress = Address.mkValidatorAddress . cdpScriptValidator

cdpCreatorScriptValidator :: CDPCreatorScriptParams -> V2.Validator
cdpCreatorScriptValidator param =
  V2.mkValidatorScript
    ( compiledValidateCDPCreatorScript
        `PlutusTx.applyCode` PlutusTx.liftCode param
    )

compiledValidateCDPCreatorScript ::
  PlutusTx.CompiledCode (CDPCreatorScriptParams -> UntypedValidator)
compiledValidateCDPCreatorScript =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||mkUntypedValidator . validateCDPCreatorScript||]
      )

compiledUntypedValidateCDPCreatorScript ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidateCDPCreatorScript =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          mkUntypedValidator
            . validateCDPCreatorScript
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedCDPCreatorValidator :: BuiltinData -> V2.Validator
untypedCDPCreatorValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidateCDPCreatorScript
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedCDPCreatorValidatorHash :: BuiltinData -> V2.ValidatorHash
untypedCDPCreatorValidatorHash = Scripts.validatorHash . untypedCDPCreatorValidator

-- serialised for use in CTL
cdpCreatorScriptCTL :: V2.Script
cdpCreatorScriptCTL =
  V2.fromCompiledCode compiledUntypedValidateCDPCreatorScript

cdpCreatorScriptAddress :: CDPCreatorScriptParams -> Ledger.Address
cdpCreatorScriptAddress = Address.mkValidatorAddress . cdpCreatorScriptValidator

iAssetPolicy :: Value.AssetClass -> V2.MintingPolicy
iAssetPolicy param =
  V2.mkMintingPolicyScript
    ( compiledMintIAsset
        `PlutusTx.applyCode` PlutusTx.liftCode (Spooky.toSpookyAssetClass param)
    )

compiledMintIAsset ::
  PlutusTx.CompiledCode
    (Spooky.AssetClass -> UntypedMintingPolicy)
compiledMintIAsset =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||Spooky.mkUntypedMintingPolicy . mkIAssetPolicy||])

compiledUntypedMintIAsset ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedMintingPolicy)
compiledUntypedMintIAsset =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          Spooky.mkUntypedMintingPolicy
            . mkIAssetPolicy
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedIAssetMintPolicy :: BuiltinData -> V2.MintingPolicy
untypedIAssetMintPolicy params =
  V2.mkMintingPolicyScript
    ( compiledUntypedMintIAsset
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedIAssetPolicyHash :: BuiltinData -> V2.MintingPolicyHash
untypedIAssetPolicyHash = Scripts.mintingPolicyHash . untypedIAssetMintPolicy

untypedIAssetPolicySymbol :: BuiltinData -> Value.CurrencySymbol
untypedIAssetPolicySymbol = Value.mpsSymbol . untypedIAssetPolicyHash

-- serialised for use in CTL
iAssetPolicyScriptCTL :: V2.Script
iAssetPolicyScriptCTL = V2.fromCompiledCode compiledUntypedMintIAsset

iAssetPolicyHash :: Value.AssetClass -> Spooky.MintingPolicyHash
iAssetPolicyHash = Spooky.mintingPolicyHash . iAssetPolicy

iAssetSymbol :: Value.AssetClass -> Value.CurrencySymbol
iAssetSymbol =
  Spooky.unSpookyCurrencySymbol . Spooky.mpsSymbol . iAssetPolicyHash
