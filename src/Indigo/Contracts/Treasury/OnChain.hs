-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.Treasury.OnChain
  ( treasuryScriptValidator,
    treasuryScriptAddress,
    validateTreasuryScript,
    treasuryScriptCTL,
    untypedTreasuryValidatorHash,
  )
where

import Indigo.Contracts.Governance.VersionRegistry.Common
  ( validateUpgradeVersion,
  )
import Indigo.Contracts.Treasury.Common
  ( TreasuryRedeemer (UpgradeVersion),
    TreasuryScriptParams (MkTreasuryScriptParams, versionRecordToken),
  )
import Indigo.Utils.Helpers (optimizeUPLC)
import Ledger qualified
import Plutus.Script.Utils.V2.Address qualified as Address
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators
  ( UntypedValidator,
    mkUntypedValidator,
  )
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude

{-# INLINEABLE validateTreasuryScript #-}
validateTreasuryScript ::
  TreasuryScriptParams ->
  () ->
  TreasuryRedeemer ->
  V2.ScriptContext ->
  Bool
validateTreasuryScript
  MkTreasuryScriptParams {versionRecordToken}
  _
  UpgradeVersion
  ctx =
    validateUpgradeVersion "Treasury" ctx versionRecordToken

treasuryScriptValidator :: TreasuryScriptParams -> V2.Validator
treasuryScriptValidator param =
  V2.mkValidatorScript
    ( compiledValidateTreasuryScript
        `PlutusTx.applyCode` PlutusTx.liftCode param
    )

compiledValidateTreasuryScript ::
  PlutusTx.CompiledCode (TreasuryScriptParams -> UntypedValidator)
compiledValidateTreasuryScript =
  optimizeUPLC
    $$(PlutusTx.compile [||mkUntypedValidator . validateTreasuryScript||])

compiledUntypedValidateTreasuryScript ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidateTreasuryScript =
  optimizeUPLC
    $$( PlutusTx.compile
          [||
          mkUntypedValidator
            . validateTreasuryScript
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedTreasuryValidator :: BuiltinData -> V2.Validator
untypedTreasuryValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidateTreasuryScript
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedTreasuryValidatorHash :: BuiltinData -> V2.ValidatorHash
untypedTreasuryValidatorHash = Scripts.validatorHash . untypedTreasuryValidator

-- serialised for use in CTL
treasuryScriptCTL :: V2.Script
treasuryScriptCTL = V2.fromCompiledCode compiledUntypedValidateTreasuryScript

treasuryScriptAddress :: TreasuryScriptParams -> Ledger.Address
treasuryScriptAddress = Address.mkValidatorAddress . treasuryScriptValidator
