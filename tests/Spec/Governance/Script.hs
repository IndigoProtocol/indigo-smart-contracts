{-# LANGUAGE TemplateHaskell #-}

module Spec.Governance.Script
  ( GovScript,
    govScript,
    PollScript,
    PollManagerScript,
    pollManagerAddress,
    pollScript,
    pollManagerScript,
    ExecuteScript,
    executeScript,
    VersionRegistryScript,
    versionRegistryScript,
    versionRecordPolicy,
    versionRecordSymbol,
    verifyUpgrade,
    upgradePolicy,
    upgradePolicyCurrency,
  )
where

import Indigo.Contracts.Governance.Execute.Common
  ( ExecuteParams,
    ExecuteRedeemer,
    Upgrade,
  )
import Indigo.Contracts.Governance.Execute.OnChain (validateExecute)
import Indigo.Contracts.Governance.Gov.Common (GovDatum, GovParams, GovRedeemer)
import Indigo.Contracts.Governance.Gov.OnChain (validateGov)
import Indigo.Contracts.Governance.Poll.Common
  ( PollManager,
    PollManagerParams,
    PollManagerRedeemer,
    PollParams,
    PollRedeemer,
    PollShard,
  )
import Indigo.Contracts.Governance.Poll.OnChain
  ( validatePoll,
    validatePollManager,
  )
import Indigo.Contracts.Governance.VersionRegistry.Common
  ( VersionRecord,
    VersionRecordMintingPolicyRedeemer,
    VersionRecordParams,
    VersionRecordRedeemer,
  )
import Indigo.Contracts.Governance.VersionRegistry.OnChain
  ( mintVersionRecord,
    validateVersionRecord,
  )
import Indigo.Utils.Helpers qualified as Helpers
import Indigo.Utils.Spooky qualified as Spooky
import Ledger qualified
import Plutus.Model.V2
  ( TypedPolicy,
    TypedValidator,
    mkTypedPolicy,
    mkTypedValidator,
    scriptCurrencySymbol,
    toBuiltinPolicy,
    toBuiltinValidator,
    validatorHash,
  )
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.Prelude

type GovScript = TypedValidator GovDatum GovRedeemer

govScript :: GovParams -> GovScript
govScript params =
  mkTypedValidator
    (compiledValidateGov `PlutusTx.applyCode` PlutusTx.liftCode params)

compiledValidateGov :: PlutusTx.CompiledCode (GovParams -> UntypedValidator)
compiledValidateGov =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||toBuiltinValidator . validateGov||])

type PollScript = TypedValidator PollShard PollRedeemer

type PollManagerScript = TypedValidator PollManager PollManagerRedeemer

pollScript :: PollParams -> PollScript
pollScript params =
  mkTypedValidator
    (compiledValidatePoll `PlutusTx.applyCode` PlutusTx.liftCode params)

pollManagerScript :: PollManagerParams -> PollManagerScript
pollManagerScript params =
  mkTypedValidator
    (compiledValidatePollManager `PlutusTx.applyCode` PlutusTx.liftCode params)

pollManagerAddress :: PollManagerParams -> Ledger.Address
pollManagerAddress =
  Ledger.scriptHashAddress . validatorHash . pollManagerScript

compiledValidatePoll :: PlutusTx.CompiledCode (PollParams -> UntypedValidator)
compiledValidatePoll =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||Spooky.mkUntypedValidator . validatePoll||])

compiledValidatePollManager ::
  PlutusTx.CompiledCode (PollManagerParams -> UntypedValidator)
compiledValidatePollManager =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||toBuiltinValidator . validatePollManager||])

type ExecuteScript = TypedValidator Upgrade ExecuteRedeemer

executeScript :: ExecuteParams -> ExecuteScript
executeScript params =
  mkTypedValidator
    (compiledValidateExecute `PlutusTx.applyCode` PlutusTx.liftCode params)

compiledValidateExecute ::
  PlutusTx.CompiledCode (ExecuteParams -> UntypedValidator)
compiledValidateExecute =
  Helpers.optimizeUPLC
    $$(PlutusTx.compile [||toBuiltinValidator . validateExecute||])

type VersionRegistryScript = TypedValidator VersionRecord VersionRecordRedeemer

versionRegistryScript :: VersionRegistryScript
versionRegistryScript =
  mkTypedValidator $
    Helpers.optimizeUPLC
      $$(PlutusTx.compile [||toBuiltinValidator validateVersionRecord||])

versionRecordPolicy ::
  VersionRecordParams -> TypedPolicy VersionRecordMintingPolicyRedeemer
versionRecordPolicy params =
  mkTypedPolicy . Helpers.optimizeUPLC $
    $$(PlutusTx.compile [||toBuiltinPolicy . mintVersionRecord||])
      `PlutusTx.applyCode` PlutusTx.liftCode params

versionRecordSymbol :: VersionRecordParams -> V2.CurrencySymbol
versionRecordSymbol = scriptCurrencySymbol . versionRecordPolicy

-- | Basic minting policy on-chain code.
-- It expects no UTxO value to be modified, and datums to be upgraded according
-- to the given function.
{-# INLINEABLE verifyUpgrade #-}
verifyUpgrade ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  (V2.Datum -> V2.Datum) ->
  () ->
  V2.ScriptContext ->
  Bool
verifyUpgrade oldValHash newValHash datumUpgrade () ctx =
  length expectedContents == length outputContents
    && null (expectedContents \\ outputContents)
    && V2.txInfoMint info
      == V2.singleton
        (Contexts.ownCurrencySymbol ctx)
        (V2.TokenName "upgrade")
        1
  where
    expectedContents :: [(V2.Value, V2.Datum)]
    expectedContents = map (datumUpgrade <$>) inputContents
    inputContents :: [(V2.Value, V2.Datum)]
    inputContents = utxoContent <$> scriptInputs
    outputContents :: [(V2.Value, V2.Datum)]
    outputContents = utxoContent <$> scriptOutputs
    scriptInputs :: [V2.TxOut]
    scriptInputs = Helpers.findAllInputsFromScript oldValHash info
    scriptOutputs :: [V2.TxOut]
    scriptOutputs =
      filter ((newAddress ==) . V2.txOutAddress) (V2.txInfoOutputs info)
    utxoContent :: V2.TxOut -> (V2.Value, V2.Datum)
    utxoContent x = case Helpers.getInlineDatum (V2.txOutDatum x) of
      Nothing -> traceError "Missing datum lookup"
      Just d -> (V2.txOutValue x, d)
    newAddress :: V2.Address
    newAddress = Ledger.scriptHashAddress newValHash
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

{-# INLINEABLE (\\) #-}
(\\) :: Eq a => [a] -> [a] -> [a]
[] \\ _ = []
(x : xs) \\ ys = if x `elem` ys then xs \\ ys else x : (xs \\ ys)

upgradePolicy ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  PlutusTx.CompiledCode (V2.Datum -> V2.Datum) ->
  TypedPolicy ()
upgradePolicy oldValHash newValHash datumUpgrade =
  mkTypedPolicy $
    $$( PlutusTx.compile
          [||\old new up -> toBuiltinPolicy (verifyUpgrade old new up)||]
      )
      `PlutusTx.applyCode` PlutusTx.liftCode oldValHash
      `PlutusTx.applyCode` PlutusTx.liftCode newValHash
      `PlutusTx.applyCode` datumUpgrade

upgradePolicyCurrency ::
  V2.ValidatorHash ->
  V2.ValidatorHash ->
  PlutusTx.CompiledCode (V2.Datum -> V2.Datum) ->
  V2.CurrencySymbol
upgradePolicyCurrency oldValHash newValHash datumUpgrade =
  scriptCurrencySymbol (upgradePolicy oldValHash newValHash datumUpgrade)
