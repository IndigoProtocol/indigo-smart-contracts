-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Indigo.Utils.Spooky.Helpers
  ( unitValue,
    hasUnitValue,
    hasPositiveValue,
    findInlinedDatumFromOutput,
    valueWithin,
    findOwnInput',
    isAuthOutput,
    noContinuingOutputs,
    hasUniqueInputWithToken,
    findUniqueInputWithToken,
    findUniqueReferenceInputWithToken,
    findUniqueInputWithTokenRef,
    findOutputFromCurrentScript,
    findOutputFromOtherScripts,
    checkOwnOutput,
    checkOwnOutputAdaGeq,
    checkOutputFromOtherScripts,
    checkOutputFromOtherScriptsAdaGeq,
    validityTimeInInterval,
    findAllInputsFromScript,
    valueOfAssetCls,
    usesSpendRedeemer,
  )
where

import Indigo.Utils.Helpers (parseDatum, serializeDatum)
import Indigo.Utils.Spooky qualified as Spooky
import Indigo.Utils.Utils (filterMap)
import Ledger qualified
import Ledger.Ada qualified as Ada
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude

{-# INLINEABLE unitValue #-}
unitValue :: Spooky.AssetClass -> Spooky.Value
unitValue ac = Spooky.assetClassValue ac 1

{-# INLINEABLE hasUnitValue #-}
hasUnitValue :: Spooky.Value -> Spooky.AssetClass -> Bool
hasUnitValue v ac = Spooky.assetClassValueOf v ac == 1

{-# INLINEABLE hasPositiveValue #-}
hasPositiveValue :: Spooky.Value -> Spooky.AssetClass -> Bool
hasPositiveValue v ac = Spooky.assetClassValueOf v ac >= 1

{-# INLINEABLE usesSpendRedeemer #-}
usesSpendRedeemer ::
  (PlutusTx.ToData a) => Spooky.TxInfo -> Spooky.TxOutRef -> a -> Bool
usesSpendRedeemer info txOut expectedRedeemer =
  case AssocMap.lookup (Spooky.Spending (Spooky.toSpooky txOut)) (Spooky.txInfoRedeemers info) of
    Nothing -> traceError "No such purpose found"
    Just re -> Ledger.getRedeemer re == PlutusTx.toBuiltinData expectedRedeemer

{-# INLINEABLE findInlinedDatumFromOutput #-}
findInlinedDatumFromOutput :: (PlutusTx.FromData a) => Spooky.TxOut -> a
findInlinedDatumFromOutput output =
  fromMaybe (traceError "Invalid datum in output") $
    case Spooky.txOutDatum output of
      Spooky.OutputDatum d -> parseDatum d
      _ -> Nothing

{-# INLINEABLE valueWithin #-}
valueWithin :: Spooky.TxInInfo -> Spooky.Value
valueWithin = Spooky.txOutValue . Spooky.txInInfoResolved

{-# INLINEABLE findOwnInput' #-}
findOwnInput' :: Spooky.ScriptContext -> Spooky.TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (Spooky.findOwnInput ctx)

{-# INLINEABLE isAuthOutput #-}
isAuthOutput :: Spooky.AssetClass -> Spooky.TxOut -> Bool
isAuthOutput = (. Spooky.txOutValue) . flip hasUnitValue

{-# INLINEABLE noContinuingOutputs #-}
noContinuingOutputs :: Spooky.ScriptContext -> Bool
noContinuingOutputs ctx = null $ Spooky.getContinuingOutputs ctx

{-# INLINEABLE hasUniqueInputWithToken #-}
hasUniqueInputWithToken :: Spooky.AssetClass -> Spooky.TxInfo -> Bool
hasUniqueInputWithToken token info =
  case filter predicate (Spooky.txInfoInputs info) of
    [_] -> True
    _ -> False
  where
    predicate = isAuthOutput token . Spooky.txInInfoResolved

{-# INLINEABLE findUniqueInputWithToken #-}
findUniqueInputWithToken :: Spooky.AssetClass -> Spooky.TxInfo -> Spooky.TxOut
findUniqueInputWithToken token info =
  snd (findUniqueInputWithTokenRef token info)

{-# INLINEABLE findUniqueReferenceInputWithToken #-}
findUniqueReferenceInputWithToken :: Spooky.AssetClass -> Spooky.TxInfo -> Spooky.TxOut
findUniqueReferenceInputWithToken token info =
  case filterMap predic Spooky.txInInfoResolved $ Spooky.txInfoReferenceInputs info of
    [o] -> o
    _ -> traceError "Expected exactly one reference input"
  where
    predic = isAuthOutput token

{-# INLINEABLE findUniqueInputWithTokenRef #-}
findUniqueInputWithTokenRef ::
  Spooky.AssetClass -> Spooky.TxInfo -> (Spooky.TxOutRef, Spooky.TxOut)
findUniqueInputWithTokenRef ac info =
  case mapMaybe refAndOut (Spooky.txInfoInputs info) of
    [x] -> x
    _ -> traceError "Expected exactly one input with single token"
  where
    refAndOut inp
      | let out = Spooky.txInInfoResolved inp,
        isAuthOutput ac out =
          Just (Spooky.txInInfoOutRef inp, out)
      | otherwise = Nothing

{-# INLINEABLE findOutputFromCurrentScript #-}
findOutputFromCurrentScript ::
  Spooky.AssetClass -> Spooky.ScriptContext -> Spooky.TxOut
findOutputFromCurrentScript token ctx =
  case filter (isAuthOutput token) $ Spooky.getContinuingOutputs ctx of
    [o] -> o
    _ -> traceError "Expected exactly one output"

{-# INLINEABLE findOutputFromOtherScripts #-}
findOutputFromOtherScripts :: Spooky.AssetClass -> Spooky.TxInfo -> Spooky.TxOut
findOutputFromOtherScripts token info =
  case filter (isAuthOutput token) $ Spooky.txInfoOutputs info of
    [o] -> o
    _ -> traceError "Expected exactly one output"

-- | Check continuing outputs for the value and datum.
-- TODO: once this function gets V2 in plutus-apps this function can be removed
-- NOTICE: This ignores hashed datum
{-# INLINEABLE checkOwnOutput #-}
checkOwnOutput ::
  (PlutusTx.ToData a) => Spooky.ScriptContext -> a -> Spooky.Value -> Bool
checkOwnOutput ctx datum value =
  let checkOutput out =
        case Spooky.txOutDatum out of
          Spooky.OutputDatum outD -> serializeDatum datum == outD
          _ -> traceError "Invalid datum to checkOwnOutput"
          && case Spooky.flattenValue (Spooky.txOutValue out - value) of
            [] -> True
            [(symbol, token, amount)] ->
              symbol == Spooky.adaSymbol
                && token == Spooky.adaToken
                && amount >= 0
                && amount <= Ada.getLovelace Ledger.minAdaTxOut
            _ -> False
   in traceIfFalse "L1" $ -- "Output constraint"
        any checkOutput (Spooky.getContinuingOutputs ctx)

-- | NOTICE: This does not check reference script at the TxOut.
-- NOTICE: This ignores hashed datum
{-# INLINEABLE checkOwnOutputAdaGeq #-}
checkOwnOutputAdaGeq ::
  (PlutusTx.ToData a) => Spooky.ScriptContext -> a -> Spooky.Value -> Bool
checkOwnOutputAdaGeq ctx datum val =
  let outputTest out =
        case Spooky.txOutDatum out of
          Spooky.OutputDatum outD -> serializeDatum datum == outD
          _ -> traceError "Invalid datum to checkOwnOutput"
          && case Spooky.flattenValue (Spooky.txOutValue out - val) of
            [] -> True
            [(symbol, token, amount)] ->
              symbol == Spooky.adaSymbol
                && token == Spooky.adaToken
                && amount >= 0
            _ -> False
   in any outputTest (Spooky.getContinuingOutputs ctx)

-- | Check if any of the outputs from the script meets the condition.
-- -- NOTICE: This ignores hashed datum
{-# INLINEABLE checkOutputFromOtherScripts #-}
checkOutputFromOtherScripts ::
  PlutusTx.ToData a =>
  Spooky.TxInfo ->
  Spooky.ValidatorHash ->
  a ->
  Spooky.Value ->
  Bool
checkOutputFromOtherScripts info valHash datum val =
  any (predicate $ serializeDatum datum) (Spooky.txInfoOutputs info)
  where
    predicate :: Ledger.Datum -> Spooky.TxOut -> Bool
    predicate
      d
      Spooky.TxOut
        { Spooky.txOutAddress',
          Spooky.txOutValue',
          Spooky.txOutDatum'
        } =
        txOutAddress' == Spooky.toSpooky (Spooky.scriptHashAddress valHash)
          && txOutValue' == Spooky.toSpooky val
          && txOutDatum' == Spooky.toSpooky (Spooky.OutputDatum d)

-- | Similar to checkOutputFromOtherScripts but allows excess ADA.
-- NOTICE: This does not check reference script at the TxOut.
{-# INLINEABLE checkOutputFromOtherScriptsAdaGeq #-}
checkOutputFromOtherScriptsAdaGeq ::
  PlutusTx.ToData a =>
  Spooky.TxInfo ->
  Spooky.ValidatorHash ->
  a ->
  Spooky.Value ->
  Bool
checkOutputFromOtherScriptsAdaGeq info valHash datum val =
  let d = serializeDatum datum
      outputs :: [Spooky.TxOut]
      outputs = Spooky.txInfoOutputs info
      outputTest :: Spooky.TxOut -> Bool
      outputTest out@Spooky.TxOut {Spooky.txOutAddress', Spooky.txOutDatum'}
        | let v = Spooky.txOutValue out =
            txOutAddress' == Spooky.toSpooky (Spooky.scriptHashAddress valHash)
              && txOutDatum' == Spooky.toSpooky (Spooky.OutputDatum d)
              && (Spooky.noAdaValue v == Spooky.noAdaValue val)
              && ( Spooky.valueOf v Spooky.adaSymbol Spooky.adaToken
                     >= Spooky.valueOf val Spooky.adaSymbol Spooky.adaToken
                 )
   in any outputTest outputs

{-# INLINEABLE validityTimeInInterval #-}
validityTimeInInterval ::
  Spooky.TxInfo -> Ledger.Interval Ledger.POSIXTime -> Bool
validityTimeInInterval info interval =
  interval `Ledger.contains` Spooky.txInfoValidRange info

{-# INLINEABLE findAllInputsFromScript #-}
findAllInputsFromScript ::
  Spooky.ValidatorHash -> Spooky.TxInfo -> [Spooky.TxOut]
findAllInputsFromScript vh info =
  let flt out
        | Spooky.Address cred _ <- Spooky.txOutAddress out,
          Spooky.ScriptCredential vh' <- Spooky.unSpooky cred =
            vh == Spooky.unSpooky vh'
      flt _ = False
   in filterMap flt Spooky.txInInfoResolved (Spooky.txInfoInputs info)

valueOfAssetCls ::
  Spooky.Value -> Spooky.CurrencySymbol -> Spooky.TokenName -> Spooky.Value
valueOfAssetCls val symbol token =
  Spooky.singleton symbol token $ Spooky.valueOf val symbol token
