-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.Collector.OnChain
  ( validateCollectorScript,
    collectorScriptAddress,
    collectorScriptValidator,
    collectorScriptCTL,
    untypedCollectorHash,
  )
where

import Indigo.Contracts.Collector.Common
import Indigo.Contracts.Governance.VersionRegistry.Common
  ( validateUpgradeVersion',
  )
import Indigo.Contracts.Staking.Common (StakingRedeemer (Distribute))
import Indigo.Utils.Helpers (optimizeUPLC)
import Indigo.Utils.Spooky qualified as Spooky
import Indigo.Utils.Spooky.Helpers qualified as Helpers
import Indigo.Utils.Utils (filterMap)
import Ledger qualified
import Plutus.Script.Utils.V2.Address qualified as Address
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (divide, fromInteger)

{- The collector script temporarily locks the fees generated from the interaction of users
with the Indigo protocol.

UTxOs locked by the script can be consumed in two different scenarios:

-----------------------------
Case 1: Collecting fees
-----------------------------

To avoid spreading the fees across many UTxOs and imposing an extra cost to
the users (or a high minimum fee), UTxOs locked by the script can be used to
accumulate fees on top of the value already held by them. In this case, there
are two conditions for a UTxO to be consumed:

  - At least the same value must be returned to the collector address from where
    the UTxO was originally consumed. This ensures that no value is substracted
    from the collector.

  - Only one UTxO locked by the script can be consumed.

-----------------------------
Case 2: Distributing fees
-----------------------------

In order to distribute the collected fees among the INDY token holders,
the collected fees must be transferred to the staking script containing
the necessary logic for a fair distribution.

The only verification needed in this case is that the Staking Manager token,
which is assumed to be locked by the right staking script at all times, is
being consumed using the correct Distribute redeemer, therefore triggering
the correct logic of the staking script.

-}
{-# INLINEABLE validateCollectorScript #-}
validateCollectorScript ::
  CollectorScriptParams ->
  () ->
  CollectorRedeemer ->
  Spooky.ScriptContext ->
  Bool
validateCollectorScript
  CollectorScriptParams {versionRecordToken}
  _
  UpgradeVersion
  ctx =
    validateUpgradeVersion' "Collector" ctx versionRecordToken
validateCollectorScript
  CollectorScriptParams {stakingManagerNFT}
  _
  Collect
  ctx =
    traceIfFalse
      "Must use Distribute redeemer or increase ADA value with just 1 input"
      ( -- Case 1
        ( Helpers.checkOwnOutputAdaGeq ctx () ownInputVal
            && numInputsOwnScript == 1
        )
          -- Case 2
          || spendStakingManagerWithDistribute stakingManagerNFT info
      )
    where
      info :: Spooky.TxInfo
      info = Spooky.scriptContextTxInfo ctx

      ownInput :: Spooky.TxOut
      ownInput = Spooky.txInInfoResolved $ Helpers.findOwnInput' ctx

      ownInputVal :: Spooky.Value
      ownInputVal = Spooky.txOutValue ownInput

      numInputsOwnScript :: Integer
      numInputsOwnScript =
        length $ Helpers.findAllInputsFromScript (Spooky.ownHash ctx) info

{-# INLINEABLE spendStakingManagerWithDistribute #-}
spendStakingManagerWithDistribute :: Spooky.AssetClass -> Spooky.TxInfo -> Bool
spendStakingManagerWithDistribute stakingManagerNFT info =
  case filterMap cond refAndOut $ Spooky.txInfoInputs info of
    [(ref, _)] ->
      AssocMap.lookup (Spooky.Spending ref) (Spooky.txInfoRedeemers info)
        == Just (V2.Redeemer $ PlutusTx.toBuiltinData Distribute)
    _ -> False
  where
    refAndOut inp = (Spooky.txInInfoOutRef' inp, Spooky.txInInfoResolved inp)
    cond (_, out') = Helpers.isAuthOutput stakingManagerNFT out'

------------------------------------------------------------------------------

collectorScriptValidator :: CollectorScriptParams -> V2.Validator
collectorScriptValidator param =
  V2.mkValidatorScript
    (compiledValidateCollector `PlutusTx.applyCode` PlutusTx.liftCode param)

compiledValidateCollector ::
  PlutusTx.CompiledCode (CollectorScriptParams -> UntypedValidator)
compiledValidateCollector =
  optimizeUPLC
    $$( PlutusTx.compile
          [||Spooky.mkUntypedValidator . validateCollectorScript||]
      )

compiledUntypedValidateCollector ::
  PlutusTx.CompiledCode (BuiltinData -> UntypedValidator)
compiledUntypedValidateCollector =
  optimizeUPLC
    $$( PlutusTx.compile
          [||
          Spooky.mkUntypedValidator
            . validateCollectorScript
            . PlutusTx.unsafeFromBuiltinData
          ||]
      )

untypedCollectorValidator :: BuiltinData -> V2.Validator
untypedCollectorValidator params =
  V2.mkValidatorScript
    ( compiledUntypedValidateCollector
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )

untypedCollectorHash :: BuiltinData -> V2.ValidatorHash
untypedCollectorHash = Scripts.validatorHash . untypedCollectorValidator

-- serialised for use in CTL
collectorScriptCTL :: V2.Script
collectorScriptCTL = V2.fromCompiledCode compiledUntypedValidateCollector

collectorScriptAddress :: CollectorScriptParams -> Ledger.Address
collectorScriptAddress = Address.mkValidatorAddress . collectorScriptValidator
