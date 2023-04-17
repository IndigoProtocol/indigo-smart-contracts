-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Indigo.Utils.Helpers
  ( oneSecond,
    oneDay,
    oneYear,
    daysDifference,
    unitValue,
    hasUnitValue,
    getInlineDatum,
    findInlinedDatumFromOutput,
    findInlinedDatumFromOutput',
    hasExpectedInlinedDatum,
    parseDatum,
    serializeDatum,
    usesSpendRedeemer,
    spendRedeemer,
    valueWithin,
    findOwnInput',
    isAuthOutput,
    isSpendingUnitValue,
    noContinuingOutputs,
    hasUniqueInputWithToken,
    findUniqueInputWithPositiveAmtOfTokens,
    findUniqueInputWithTokenRef,
    findUniqueInputWithPositiveAmtOfTokensRef,
    findUniqueReferenceInputWithToken,
    findUniqueInputWithToken,
    findUniqueOutputFromCurrentScript,
    findUniqueOutputFromOtherScripts,
    checkOutput,
    checkOutputAdaGeq,
    checkOutputDatum,
    checkOwnOutput,
    getContinuingOutputsNoStaking,
    checkOwnOutputNoStaking,
    checkOwnOutputAdaGeq,
    checkOutputFromOtherScripts,
    checkOutputFromOtherScriptsWithStakingCredential,
    checkOutputFromOtherScriptsAdaGeq,
    optimizeUPLC,
    validityTimeInInterval,
    findAllInputsFromScript,
    findAllOutputsToAddress,
    valueOfAssetCls',
    valueOfAssetCls,
    hasPositiveValue,
    getTokenName,
    lovelacesAmount,
  )
where

import Indigo.Utils.Utils (filterMap)
import Ledger (CurrencySymbol, TokenName, Value)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Value (valueOf)
import Ledger.Value qualified as Value
#if defined(PLUTONOMY)
import Plutonomy.Optimize (defaultOptimizerOptions)

#if !defined(DEBUG)
import Plutonomy.Optimize
  ( OptimizerOptions (ooTraceRewrite),
    TraceRewrite (TraceRemove),
  )
#endif

import Plutonomy.UPLC (HasUPLC, optimizeUPLCWith)
#endif

import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts as Contexts
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude

{-# INLINEABLE oneSecond #-}
oneSecond :: Ledger.POSIXTime
oneSecond = Ledger.POSIXTime 1_000

{-# INLINEABLE oneDay #-}
oneDay :: Ledger.POSIXTime
oneDay = Ledger.POSIXTime 86_400_000

{-# INLINEABLE oneYear #-}
oneYear :: Ledger.POSIXTime
oneYear = Ledger.POSIXTime 31_536_000_000

{-# INLINEABLE daysDifference #-}
daysDifference :: Ledger.POSIXTime -> Ledger.POSIXTime -> Integer
daysDifference (Ledger.POSIXTime a) (Ledger.POSIXTime b) =
  (a - b) `divide` Ledger.getPOSIXTime oneDay

lovelacesAmount :: Ledger.Value -> Integer
lovelacesAmount = Ada.getLovelace . Ada.fromValue

{-# INLINEABLE unitValue #-}
unitValue :: Value.AssetClass -> Ledger.Value
unitValue ac = Value.assetClassValue ac 1

{-# INLINEABLE hasUnitValue #-}
hasUnitValue :: Ledger.Value -> Value.AssetClass -> Bool
hasUnitValue v ac = Value.assetClassValueOf v ac == 1

{-# INLINEABLE isSpendingUnitValue #-}
isSpendingUnitValue :: V2.TxInfo -> Value.AssetClass -> Bool
isSpendingUnitValue = hasUnitValue . Contexts.valueSpent

{-# INLINEABLE hasPositiveValue #-}
hasPositiveValue :: Value.AssetClass -> Ledger.Value -> Bool
hasPositiveValue ac v = Value.assetClassValueOf v ac >= 1

{-# INLINEABLE parseDatum #-}
parseDatum :: (PlutusTx.FromData a) => V2.Datum -> Maybe a
parseDatum da = PlutusTx.fromBuiltinData (Ledger.getDatum da)

{-# INLINEABLE serializeDatum #-}
serializeDatum :: (PlutusTx.ToData a) => a -> V2.Datum
serializeDatum d = V2.Datum (PlutusTx.toBuiltinData d)

{-# INLINEABLE getInlineDatum #-}
getInlineDatum :: V2.OutputDatum -> Maybe V2.Datum
getInlineDatum d = case d of
  V2.OutputDatum datum -> Just datum
  _ -> Nothing

{-# INLINEABLE hasExpectedInlinedDatum #-}
hasExpectedInlinedDatum :: (PlutusTx.ToData a) => V2.TxOut -> a -> Bool
hasExpectedInlinedDatum V2.TxOut {txOutDatum = V2.OutputDatum outD} datum =
  serializeDatum datum == outD
hasExpectedInlinedDatum _ _ = False

{-# INLINEABLE usesSpendRedeemer #-}
usesSpendRedeemer ::
  (PlutusTx.ToData a) => V2.TxInfo -> V2.TxOutRef -> a -> Bool
usesSpendRedeemer info txOut expectedRedeemer =
  case AssocMap.lookup (V2.Spending txOut) (V2.txInfoRedeemers info) of
    Nothing -> traceError "No such purpose found"
    Just re -> V2.getRedeemer re == PlutusTx.toBuiltinData expectedRedeemer

{-# INLINEABLE spendRedeemer #-}
spendRedeemer :: (PlutusTx.FromData a) => V2.TxInfo -> V2.TxOutRef -> a
spendRedeemer info ref =
  case AssocMap.lookup (V2.Spending ref) (V2.txInfoRedeemers info)
    >>= PlutusTx.fromBuiltinData . V2.getRedeemer of
    Nothing -> traceError "Could not find redeemer"
    Just re -> re

{-# INLINEABLE findInlinedDatumFromOutput #-}
findInlinedDatumFromOutput :: (PlutusTx.FromData a) => V2.TxOut -> a
findInlinedDatumFromOutput output =
  fromMaybe
    (traceError "Invalid inline datum in output")
    (getInlineDatum (V2.txOutDatum output) >>= parseDatum)

{-# INLINEABLE findInlinedDatumFromOutput' #-}
findInlinedDatumFromOutput' :: (PlutusTx.FromData a) => V2.TxOut -> Maybe a
findInlinedDatumFromOutput' output =
  getInlineDatum (V2.txOutDatum output) >>= parseDatum

{-# INLINEABLE valueWithin #-}
valueWithin :: V2.TxInInfo -> Ledger.Value
valueWithin = V2.txOutValue . V2.txInInfoResolved

{-# INLINEABLE findOwnInput' #-}
findOwnInput' :: V2.ScriptContext -> V2.TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (Contexts.findOwnInput ctx)

{-# INLINEABLE isAuthOutput #-}
isAuthOutput :: Value.AssetClass -> V2.TxOut -> Bool
isAuthOutput = (. V2.txOutValue) . flip hasUnitValue

{-# INLINEABLE noContinuingOutputs #-}
noContinuingOutputs :: V2.ScriptContext -> Bool
noContinuingOutputs ctx = null $ Contexts.getContinuingOutputs ctx

{-# INLINEABLE hasUniqueInputWithToken #-}
hasUniqueInputWithToken :: Value.AssetClass -> V2.TxInfo -> Bool
hasUniqueInputWithToken token info =
  case filterMap predicate V2.txInInfoResolved (V2.txInfoInputs info) of
    [_] -> True
    _ -> False
  where
    predicate = isAuthOutput token

{-# INLINEABLE findUniqueInputWithToken #-}
findUniqueInputWithToken :: Value.AssetClass -> V2.TxInfo -> V2.TxOut
findUniqueInputWithToken token info =
  case filterMap predicate V2.txInInfoResolved (V2.txInfoInputs info) of
    [o] -> o
    _ -> traceError "Expected exactly one input with single token"
  where
    predicate = isAuthOutput token

-- | The amount of tokens in the UTXO can be 1 or more.
{-# INLINEABLE findUniqueInputWithPositiveAmtOfTokens #-}
findUniqueInputWithPositiveAmtOfTokens ::
  Value.AssetClass -> V2.TxInfo -> V2.TxOut
findUniqueInputWithPositiveAmtOfTokens token info =
  case filterMap predicate V2.txInInfoResolved $ V2.txInfoInputs info of
    [o] -> o
    _ -> traceError "Expected exactly one input with positive amount of tokens"
  where
    predicate = hasPositiveValue token . V2.txOutValue

{-# INLINEABLE findUniqueInputWithTokenRef #-}
findUniqueInputWithTokenRef ::
  Value.AssetClass -> V2.TxInfo -> (V2.TxOutRef, V2.TxOut)
findUniqueInputWithTokenRef ac info =
  case filterMap predicate refAndOut $ V2.txInfoInputs info of
    [o] -> o
    _ -> traceError "Expected exactly one input with single token"
  where
    predicate (_, o) = isAuthOutput ac o
    refAndOut !inp = (V2.txInInfoOutRef inp, V2.txInInfoResolved inp)

-- | The amount of tokens in the UTXO can be 1 or more.
{-# INLINEABLE findUniqueInputWithPositiveAmtOfTokensRef #-}
findUniqueInputWithPositiveAmtOfTokensRef ::
  Value.AssetClass -> V2.TxInfo -> (V2.TxOutRef, V2.TxOut)
findUniqueInputWithPositiveAmtOfTokensRef ac info =
  case filterMap predicate refAndOut $ V2.txInfoInputs info of
    [(ref, out)] -> (ref, out)
    _ -> traceError "Expected exactly one input with positive amount of tokens"
  where
    predicate (_, o) = hasPositiveValue ac (V2.txOutValue o)
    refAndOut !inp = (V2.txInInfoOutRef inp, V2.txInInfoResolved inp)

{-# INLINEABLE findUniqueReferenceInputWithToken #-}
findUniqueReferenceInputWithToken :: Value.AssetClass -> V2.TxInfo -> V2.TxOut
findUniqueReferenceInputWithToken token info =
  case filterMap predic V2.txInInfoResolved $ V2.txInfoReferenceInputs info of
    [o] -> o
    _ -> traceError "Expected exactly one reference input"
  where
    predic = isAuthOutput token

{-# INLINEABLE findUniqueOutputFromCurrentScript #-}
findUniqueOutputFromCurrentScript ::
  Value.AssetClass -> V2.ScriptContext -> V2.TxOut
findUniqueOutputFromCurrentScript token ctx =
  case filter (isAuthOutput token) $ Contexts.getContinuingOutputs ctx of
    [o] -> o
    _ -> traceError "Expected exactly one output"

{-# INLINEABLE findUniqueOutputFromOtherScripts #-}
findUniqueOutputFromOtherScripts :: Value.AssetClass -> V2.TxInfo -> V2.TxOut
findUniqueOutputFromOtherScripts token info =
  case filter (isAuthOutput token) $ V2.txInfoOutputs info of
    [o] -> o
    _ -> traceError "Expected exactly one output"

-- | Check continuing outputs for the value and datum.
-- NOTICE: This does not check reference script at the TxOut.
-- NOTICE: This function allows the continuation of the validator hash
-- with the staking credential continued from the input.
-- NOTICE: This ignores hashed datum
{-# INLINEABLE checkOwnOutput #-}
checkOwnOutput ::
  (PlutusTx.ToData a) => V2.ScriptContext -> a -> Ledger.Value -> Bool
checkOwnOutput ctx =
  (checkOwnOutputConstraint .) . Constraints.ScriptOutputConstraint
  where
    checkOwnOutputConstraint ::
      (PlutusTx.ToData o) => Constraints.ScriptOutputConstraint o -> Bool
    checkOwnOutputConstraint
      Constraints.ScriptOutputConstraint
        { Constraints.ocDatum,
          Constraints.ocValue
        } =
        let datum = serializeDatum ocDatum
         in traceIfFalse "L1" $ -- "Output constraint"
              any
                (checkOutput datum ocValue)
                (Contexts.getContinuingOutputs ctx)

-- | Get all the outputs that pay to the same script address
-- we are currently spending from, if any.
-- This is a modified version of the original `getContinuingOutputs`.
-- This version does not allow staking credentials on output.
{-# INLINEABLE getContinuingOutputsNoStaking #-}
getContinuingOutputsNoStaking :: ScriptContext -> [TxOut]
getContinuingOutputsNoStaking ctx
  | Just TxInInfo {txInInfoResolved = TxOut {txOutAddress}} <-
      findOwnInput ctx =
      filter (f txOutAddress) (txInfoOutputs $ scriptContextTxInfo ctx)
  where
    f addr TxOut {txOutAddress = otherAddress} =
      Ledger.toValidatorHash addr
        == Ledger.toValidatorHash otherAddress
        && isNothing (V2.addressStakingCredential otherAddress)
getContinuingOutputsNoStaking _ = traceError "Lf" -- "Can't get any continuing outputs"

-- | Check continuing outputs for the value and datum.
-- NOTICE: This ignores hashed datum
-- NOTICE: This checks validator hashes and does not allow a staking credential.
{-# INLINEABLE checkOwnOutputNoStaking #-}
checkOwnOutputNoStaking ::
  (PlutusTx.ToData a) => V2.ScriptContext -> a -> Ledger.Value -> Bool
checkOwnOutputNoStaking ctx =
  (checkOwnOutputConstraint .) . Constraints.ScriptOutputConstraint
  where
    checkOwnOutputConstraint ::
      (PlutusTx.ToData o) => Constraints.ScriptOutputConstraint o -> Bool
    checkOwnOutputConstraint
      Constraints.ScriptOutputConstraint
        { Constraints.ocDatum,
          Constraints.ocValue
        } =
        let datum = serializeDatum ocDatum
         in traceIfFalse "L1" $ -- "Output constraint"
              any
                (checkOutput datum ocValue)
                (getContinuingOutputsNoStaking ctx)

-- | Check continuing outputs for the value and datum, ignoring the ADA value.
-- NOTICE: This does not check reference script at the TxOut.
-- NOTICE: This ignores hashed datum
{-# INLINEABLE checkOwnOutputAdaGeq #-}
checkOwnOutputAdaGeq ::
  (PlutusTx.ToData a) => V2.ScriptContext -> a -> Ledger.Value -> Bool
checkOwnOutputAdaGeq ctx datum val =
  let d = serializeDatum datum
      outputVals = Contexts.getContinuingOutputs ctx
   in any (checkOutputAdaGeq d val) outputVals

-- | Check a single output for the expected datum and value.
-- NOTICE: This does not check reference script at the TxOut.
-- NOTICE: This ignores hashed datum
{-# INLINEABLE checkOutput #-}
checkOutput :: V2.Datum -> Value -> TxOut -> Bool
checkOutput d val TxOut {txOutValue, txOutDatum = V2.OutputDatum outDatum} =
  case Value.flattenValue (txOutValue - val) of
    [] -> True
    [(symbol, token, amount)] ->
      symbol == Ada.adaSymbol
        && token == Ada.adaToken
        && amount >= 0
        && amount <= Ada.getLovelace Ledger.minAdaTxOut
    _ -> trace "Value mismatch in checkOutput" False
    && (d == outDatum)
checkOutput _ _ _ = trace "No datum in checkOutput" False

-- | Check a single output for the expected datum and value,
-- ignoring the ADA value.
-- NOTICE: This does not check reference script at the TxOut.
-- NOTICE: This ignores hashed datum
{-# INLINEABLE checkOutputAdaGeq #-}
checkOutputAdaGeq :: V2.Datum -> Value -> TxOut -> Bool
checkOutputAdaGeq
  d
  val
  TxOut {txOutValue, txOutDatum = V2.OutputDatum outDatum} =
    case Value.flattenValue (txOutValue - val) of
      [] -> True
      [(symbol, token, amount)] ->
        symbol == Ada.adaSymbol
          && token == Ada.adaToken
          && amount >= 0
      _ -> trace "Value mismatch in checkOutputAdaGeq" False
      && (d == outDatum)
checkOutputAdaGeq _ _ _ = trace "No datum in checkOutputAdaGeq" False

-- | Check a single output for the expected datum.
{-# INLINEABLE checkOutputDatum #-}
checkOutputDatum :: PlutusTx.ToData a => a -> TxOut -> Bool
checkOutputDatum datum TxOut {txOutDatum = V2.OutputDatum outD} =
  serializeDatum datum == outD
checkOutputDatum _ _ = False

-- | Check any of the outputs going to the script
-- for the expected datum and value.
-- NOTICE: This does not check reference script at the TxOut.
-- NOTICE: This does not allow a staking credential.
-- NOTICE: This ignores hashed datum
{-# INLINEABLE checkOutputFromOtherScripts #-}
checkOutputFromOtherScripts ::
  PlutusTx.ToData a =>
  V2.TxInfo ->
  V2.ValidatorHash ->
  a ->
  Ledger.Value ->
  Bool
checkOutputFromOtherScripts info valHash datum val =
  any predicate (V2.txInfoOutputs info)
  where
    predicate :: V2.TxOut -> Bool
    predicate out@V2.TxOut {txOutAddress} =
      txOutAddress == Ledger.scriptHashAddress valHash && checkOutput d val out
    d = serializeDatum datum

-- | Check any of the outputs going to the script
-- for the expected datum and value.
-- NOTICE: This does not check reference script at the TxOut.
-- NOTICE: This allows a staking credential.
-- NOTICE: This ignores hashed datum
{-# INLINEABLE checkOutputFromOtherScriptsWithStakingCredential #-}
checkOutputFromOtherScriptsWithStakingCredential ::
  PlutusTx.ToData a =>
  V2.TxInfo ->
  V2.ValidatorHash ->
  a ->
  Ledger.Value ->
  Bool
checkOutputFromOtherScriptsWithStakingCredential info valHash datum val =
  any predicate (V2.txInfoOutputs info)
  where
    predicate :: V2.TxOut -> Bool
    predicate out@V2.TxOut {txOutAddress} =
      Ledger.toValidatorHash txOutAddress
        == Just valHash
        && checkOutput d val out
    d = serializeDatum datum

-- | Check any of the outputs going to the script
-- for the expected datum and value, ignoring the ADA value.
-- NOTICE: This does not check reference script at the TxOut.
-- NOTICE: This ignores hashed datum
{-# INLINEABLE checkOutputFromOtherScriptsAdaGeq #-}
checkOutputFromOtherScriptsAdaGeq ::
  PlutusTx.ToData a =>
  V2.TxInfo ->
  V2.ValidatorHash ->
  a ->
  Ledger.Value ->
  Bool
checkOutputFromOtherScriptsAdaGeq info valHash datum val =
  let d = serializeDatum datum
      outputs :: [V2.TxOut]
      outputs = V2.txInfoOutputs info
      outputTest :: V2.TxOut -> Bool
      outputTest out =
        V2.txOutAddress out
          == Ledger.scriptHashAddress valHash
          && checkOutputAdaGeq d val out
   in any outputTest outputs

{-# INLINEABLE validityTimeInInterval #-}
validityTimeInInterval :: V2.TxInfo -> Ledger.Interval Ledger.POSIXTime -> Bool
validityTimeInInterval info interval =
  interval `Ledger.contains` V2.txInfoValidRange info

{-# INLINEABLE findAllInputsFromScript #-}
findAllInputsFromScript :: V2.ValidatorHash -> V2.TxInfo -> [V2.TxOut]
findAllInputsFromScript vh info =
  let flt V2.TxOut {txOutAddress = V2.Address (V2.ScriptCredential vh') _} =
        vh == vh'
      flt _ = False
   in filterMap flt V2.txInInfoResolved (V2.txInfoInputs info)

-- | Contrary to @V2.scriptOutputsAt@ this doesn't allow
-- staking credential to be present in the script address.
{-# INLINEABLE findAllOutputsToAddress #-}
findAllOutputsToAddress :: V2.ValidatorHash -> V2.TxInfo -> [V2.TxOut]
findAllOutputsToAddress vh info =
  let flt
        V2.TxOut {txOutAddress = V2.Address (V2.ScriptCredential vh') Nothing} =
          vh == vh'
      flt _ = False
   in filter flt (V2.txInfoOutputs info)

{-# INLINEABLE valueOfAssetCls' #-}
valueOfAssetCls' :: Value -> CurrencySymbol -> TokenName -> Value
valueOfAssetCls' val symbol token =
  Value.singleton symbol token $ valueOf val symbol token

{-# INLINEABLE valueOfAssetCls #-}
valueOfAssetCls :: Value -> Value.AssetClass -> Value
valueOfAssetCls val asset =
  Value.assetClassValue asset $ Value.assetClassValueOf val asset

{-# INLINEABLE getTokenName #-}
getTokenName :: Value.AssetClass -> TokenName
getTokenName = snd . Value.unAssetClass

#if defined(PLUTONOMY)

optimizeUPLC :: HasUPLC a => a -> a
optimizeUPLC = optimizeUPLCWith defaultOptimizerOptions
#if !defined(DEBUG)
  {ooTraceRewrite = Just TraceRemove}
#endif

#else

optimizeUPLC :: a -> a
optimizeUPLC = id

#endif
