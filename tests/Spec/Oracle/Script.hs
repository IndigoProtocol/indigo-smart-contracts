{-# LANGUAGE TemplateHaskell #-}

module Spec.Oracle.Script (OracleScript, oracleScript) where

import Indigo.Contracts.Oracle.Common
  ( OracleDatum,
    OracleParams,
    OracleRedeemer,
  )
import Indigo.Contracts.Oracle.OnChain (validateOracle)
import Indigo.Utils.Helpers (optimizeUPLC)
import Indigo.Utils.Spooky qualified as Spooky
import Plutus.Model.V2 (TypedValidator, mkTypedValidator)
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import PlutusTx qualified
import PlutusTx.Prelude

type OracleScript = TypedValidator OracleDatum OracleRedeemer

oracleScript :: OracleParams -> OracleScript
oracleScript params =
  mkTypedValidator
    (compiledValidateOracle `PlutusTx.applyCode` PlutusTx.liftCode params)

compiledValidateOracle ::
  PlutusTx.CompiledCode (OracleParams -> UntypedValidator)
compiledValidateOracle =
  optimizeUPLC
    $$(PlutusTx.compile [||Spooky.mkUntypedValidator . validateOracle||])
