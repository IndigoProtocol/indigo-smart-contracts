{-# LANGUAGE TemplateHaskell #-}

module Spec.Collector.Script (CollectorScript, collectorScript) where

import Indigo.Contracts.Collector.Common
  ( CollectorRedeemer,
    CollectorScriptParams,
  )
import Indigo.Contracts.Collector.OnChain (validateCollectorScript)
import Indigo.Utils.Helpers (optimizeUPLC)
import Indigo.Utils.Spooky qualified as Spooky
import Plutus.Model.V2 (TypedValidator, mkTypedValidator)
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import PlutusTx qualified
import PlutusTx.Prelude

type CollectorScript = TypedValidator () CollectorRedeemer

collectorScript :: CollectorScriptParams -> CollectorScript
collectorScript params =
  mkTypedValidator
    (compiledValidateCollector `PlutusTx.applyCode` PlutusTx.liftCode params)

compiledValidateCollector ::
  PlutusTx.CompiledCode (CollectorScriptParams -> UntypedValidator)
compiledValidateCollector =
  optimizeUPLC
    $$( PlutusTx.compile
          [||Spooky.mkUntypedValidator . validateCollectorScript||]
      )
