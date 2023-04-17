-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.Oracle.OnChain
  ( validateOracle,
    oracleValidator,
    oracleAddress,
    oracleScriptCTL,
    untypedOracleValidatorHash,
  )
where

import Indigo.Contracts.Oracle.Common
import Indigo.Utils.Helpers (optimizeUPLC)
import Indigo.Utils.Spooky qualified as Spooky
import Indigo.Utils.Spooky.Helpers qualified as Helpers
import Ledger qualified
import Ledger.Interval qualified as Interval
import Plutus.Script.Utils.V2.Address qualified as Address
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude

{-# INLINEABLE validateOracle #-}
validateOracle ::
  OracleParams ->
  OracleDatum ->
  OracleRedeemer ->
  Spooky.ScriptContext ->
  Bool
validateOracle
  OracleParams {opOwner, opBiasTime, opExpirationTime}
  _
  (FeedPrice now price)
  ctx =
    traceIfFalse
      "The transaction is not signed by oracle owner"
      (Spooky.txSignedBy info opOwner)
      && traceIfFalse
        "output does not match"
        ( Helpers.checkOwnOutput @OracleDatum
            ctx
            (MkOracleDatum price (odExpiration outputDatum))
            oracleInputValue
        )
      && traceIfFalse "Price must be positive" (price > zero)
      && traceIfFalse
        "Expiration time is not properly set"
        ( isExpirationProperlySet
            && Helpers.validityTimeInInterval
              info
              (Interval.interval (now - opBiasTime) (now + opBiasTime))
        )
    where
      info :: Spooky.TxInfo
      info = Spooky.scriptContextTxInfo ctx

      ownInput :: Spooky.TxInInfo
      ownInput = Helpers.findOwnInput' ctx

      ownOutput :: Spooky.TxOut
      ownOutput = case Spooky.getContinuingOutputs ctx of
        [o] -> o
        _ -> traceError "expected only a single continuing output"

      oracleInputValue :: Spooky.Value
      oracleInputValue = Helpers.valueWithin ownInput

      outputDatum :: OracleDatum
      outputDatum = Helpers.findInlinedDatumFromOutput ownOutput

      isExpirationProperlySet :: Bool
      isExpirationProperlySet =
        case Ledger.ivTo $ Spooky.txInfoValidRange info of
          Ledger.UpperBound (Ledger.Finite currentTimeApprox) _ ->
            currentTimeApprox + opExpirationTime >= odExpiration outputDatum
              && currentTimeApprox <= odExpiration outputDatum
          _ -> False

oracleValidator :: OracleParams -> V2.Validator
oracleValidator param =
  V2.mkValidatorScript
    (compiledValidateOracle `PlutusTx.applyCode` PlutusTx.liftCode param)

compiledValidateOracle ::
  PlutusTx.CompiledCode (OracleParams -> UntypedValidator)
compiledValidateOracle =
  optimizeUPLC
    $$(PlutusTx.compile [||Spooky.mkUntypedValidator . validateOracle||])

compiledUntypedValidateOracle ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidateOracle =
  optimizeUPLC
    $$( PlutusTx.compile
          [||
          Spooky.mkUntypedValidator
            . validateOracle
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedOracleValidator :: BuiltinData -> V2.Validator
untypedOracleValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidateOracle
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedOracleValidatorHash :: BuiltinData -> V2.ValidatorHash
untypedOracleValidatorHash = Scripts.validatorHash . untypedOracleValidator

-- serialised for use in CTL
oracleScriptCTL :: V2.Script
oracleScriptCTL = V2.fromCompiledCode compiledUntypedValidateOracle

oracleAddress :: OracleParams -> V2.Address
oracleAddress = Address.mkValidatorAddress . oracleValidator
