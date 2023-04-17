-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.Governance.VersionRegistry.OnChain
  ( validateVersionRecord,
    mintVersionRecord,
    versionRecordTokenPolicy,
    versionRegistryValidator,
    versionRegistryValidatorAddress,
    versionRegistryValidatorHash,
    versionRecordTokenPolicyScriptCTL,
    versionRegistryScriptCTL,
    untypedVersionRecordMintSymbol,
    untypedVersionRegistryValidatorHash,
  )
where

import Indigo.Contracts.Governance.VersionRegistry.Common
  ( VersionRecord,
    VersionRecordMintingPolicyRedeemer (AddRecord),
    VersionRecordParams (VersionRecordParams, upgradeToken),
    VersionRecordRedeemer,
  )
import Indigo.Utils.Helpers qualified as Helpers
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Address qualified as Address
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies
  ( UntypedMintingPolicy,
    mkUntypedMintingPolicy,
  )
import Plutus.Script.Utils.V2.Typed.Scripts.Validators
  ( UntypedValidator,
    mkUntypedValidator,
  )
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude (seq)

{-# INLINEABLE validateVersionRecord #-}
validateVersionRecord ::
  VersionRecord ->
  VersionRecordRedeemer ->
  V2.ScriptContext ->
  Bool
validateVersionRecord _ _ _ = False

{-# INLINEABLE mintVersionRecord #-}
mintVersionRecord ::
  VersionRecordParams ->
  VersionRecordMintingPolicyRedeemer ->
  V2.ScriptContext ->
  Bool
mintVersionRecord VersionRecordParams {upgradeToken} AddRecord ctx =
  Helpers.findUniqueInputWithToken upgradeToken info `seq` True
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

versionRecordTokenPolicy :: VersionRecordParams -> V2.MintingPolicy
versionRecordTokenPolicy params =
  V2.mkMintingPolicyScript
    (compiledMintVersionRecord `PlutusTx.applyCode` PlutusTx.liftCode params)

compiledMintVersionRecord ::
  PlutusTx.CompiledCode (VersionRecordParams -> UntypedMintingPolicy)
compiledMintVersionRecord =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||mkUntypedMintingPolicy . mintVersionRecord||])

compiledUntypedMintVersionRecord ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedMintingPolicy)
compiledUntypedMintVersionRecord =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||
          mkUntypedMintingPolicy
            . mintVersionRecord
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedVersionRecordMintPolicy :: BuiltinData -> V2.MintingPolicy
untypedVersionRecordMintPolicy params =
  V2.mkMintingPolicyScript
    ( compiledUntypedMintVersionRecord
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedVersionRecordMintPolicyHash :: BuiltinData -> V2.MintingPolicyHash
untypedVersionRecordMintPolicyHash = PV2.mintingPolicyHash . untypedVersionRecordMintPolicy

untypedVersionRecordMintSymbol :: BuiltinData -> Value.CurrencySymbol
untypedVersionRecordMintSymbol = Value.mpsSymbol . untypedVersionRecordMintPolicyHash

-- serialised for use in CTL
versionRecordTokenPolicyScriptCTL :: V2.Script
versionRecordTokenPolicyScriptCTL =
  V2.fromCompiledCode compiledUntypedMintVersionRecord

versionRegistryValidator :: V2.Validator
versionRegistryValidator = V2.mkValidatorScript compiledValidateVersionRecord

compiledValidateVersionRecord :: PlutusTx.CompiledCode UntypedValidator
compiledValidateVersionRecord =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||mkUntypedValidator validateVersionRecord||])

untypedVersionRegistryValidator :: V2.Validator
untypedVersionRegistryValidator = V2.mkValidatorScript compiledValidateVersionRecord

untypedVersionRegistryValidatorHash :: V2.ValidatorHash
untypedVersionRegistryValidatorHash = PV2.validatorHash untypedVersionRegistryValidator

-- serialised for use in CTL
versionRegistryScriptCTL :: V2.Script
versionRegistryScriptCTL = V2.fromCompiledCode compiledValidateVersionRecord

versionRegistryValidatorHash :: V2.ValidatorHash
versionRegistryValidatorHash = PV2.validatorHash versionRegistryValidator

versionRegistryValidatorAddress :: Ledger.Address
versionRegistryValidatorAddress =
  Address.mkValidatorAddress versionRegistryValidator
