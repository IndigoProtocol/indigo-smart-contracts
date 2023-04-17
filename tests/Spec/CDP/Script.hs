{-# LANGUAGE TemplateHaskell #-}

module Spec.CDP.Script
  ( CDPScript,
    CDPCreatorScript,
    cdpScript,
    cdpCreatorScript,
    iAssetPolicy,
    iAssetSymbol,
  )
where

import Indigo.Contracts.CDP.Common
  ( CDPCreatorDatum,
    CDPCreatorRedeemer,
    CDPCreatorScriptParams,
    CDPDatum,
    CDPRedeemer,
    CDPScriptParams,
  )
import Indigo.Contracts.CDP.OnChain
  ( mkIAssetPolicy,
    validateCDPCreatorScript,
    validateCDPScript,
  )
import Indigo.Utils.Helpers (optimizeUPLC)
import Indigo.Utils.Spooky qualified as Spooky
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Model.V2
  ( TypedPolicy,
    TypedValidator,
    mkTypedPolicy,
    mkTypedValidator,
    scriptCurrencySymbol,
    toBuiltinValidator,
  )
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies
  ( UntypedMintingPolicy,
  )
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import PlutusTx qualified
import PlutusTx.Prelude

type CDPScript = TypedValidator CDPDatum CDPRedeemer

cdpScript :: CDPScriptParams -> CDPScript
cdpScript params =
  mkTypedValidator
    (compiledValidateCDPScript `PlutusTx.applyCode` PlutusTx.liftCode params)

compiledValidateCDPScript ::
  PlutusTx.CompiledCode (CDPScriptParams -> UntypedValidator)
compiledValidateCDPScript =
  optimizeUPLC $$(PlutusTx.compile [||toBuiltinValidator . validateCDPScript||])

type CDPCreatorScript = TypedValidator CDPCreatorDatum CDPCreatorRedeemer

cdpCreatorScript :: CDPCreatorScriptParams -> CDPCreatorScript
cdpCreatorScript params =
  mkTypedValidator
    ( compiledValidateCDPCreatorScript
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

compiledValidateCDPCreatorScript ::
  PlutusTx.CompiledCode (CDPCreatorScriptParams -> UntypedValidator)
compiledValidateCDPCreatorScript =
  optimizeUPLC
    $$(PlutusTx.compile [||toBuiltinValidator . validateCDPCreatorScript||])

iAssetPolicy :: Value.AssetClass -> TypedPolicy ()
iAssetPolicy params =
  mkTypedPolicy
    ( compiledMintIAsset
        `PlutusTx.applyCode` PlutusTx.liftCode
          (Spooky.toSpookyAssetClass params)
    )

compiledMintIAsset ::
  PlutusTx.CompiledCode (Spooky.AssetClass -> UntypedMintingPolicy)
compiledMintIAsset =
  optimizeUPLC
    $$(PlutusTx.compile [||Spooky.mkUntypedMintingPolicy . mkIAssetPolicy||])

iAssetSymbol :: Value.AssetClass -> Ledger.CurrencySymbol
iAssetSymbol = scriptCurrencySymbol . iAssetPolicy
