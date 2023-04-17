{-# LANGUAGE TemplateHaskell #-}

module Spec.StabilityPool.Script
  ( StabilityPoolScript,
    stabilityPoolScript,
  )
where

import Indigo.Contracts.StabilityPool.Common
  ( StabilityDatum,
    StabilityPoolParams,
    StabilityPoolRedeemer,
  )
import Indigo.Contracts.StabilityPool.OnChain (validateStabilityPool)
import Indigo.Utils.Helpers (optimizeUPLC)
import Plutus.Model.V2 (TypedValidator, mkTypedValidator, toBuiltinValidator)
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import PlutusTx qualified
import PlutusTx.Prelude

type StabilityPoolScript = TypedValidator StabilityDatum StabilityPoolRedeemer

stabilityPoolScript :: StabilityPoolParams -> StabilityPoolScript
stabilityPoolScript params =
  mkTypedValidator
    ( compiledValidateStabilityPool
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

compiledValidateStabilityPool ::
  PlutusTx.CompiledCode (StabilityPoolParams -> UntypedValidator)
compiledValidateStabilityPool =
  optimizeUPLC
    $$(PlutusTx.compile [||toBuiltinValidator . validateStabilityPool||])
