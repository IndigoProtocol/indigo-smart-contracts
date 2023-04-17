{-# LANGUAGE TemplateHaskell #-}

module Spec.Treasury.Script (TreasuryScript, treasuryScript) where

import Indigo.Contracts.Treasury.Common (TreasuryRedeemer, TreasuryScriptParams)
import Indigo.Contracts.Treasury.OnChain (validateTreasuryScript)
import Plutus.Model.V2 (TypedValidator, mkTypedValidator, toBuiltinValidator)
import PlutusTx qualified
import PlutusTx.Prelude

type TreasuryScript = TypedValidator () TreasuryRedeemer

treasuryScript :: TreasuryScriptParams -> TreasuryScript
treasuryScript params =
  mkTypedValidator $
    $$(PlutusTx.compile [||toBuiltinValidator . validateTreasuryScript||])
      `PlutusTx.applyCode` PlutusTx.liftCode params
