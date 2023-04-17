{-# LANGUAGE TemplateHaskell #-}

module Spec.Staking.Script
  ( StakingScript,
    stakingScript,
    alwaysPassingValidator,
  )
where

import Indigo.Contracts.Staking.Common
  ( StakingDatum,
    StakingParams,
    StakingRedeemer,
  )
import Indigo.Contracts.Staking.OnChain (validateStaking)
import Indigo.Utils.Helpers (optimizeUPLC)
import Plutus.Model.V2
  ( TypedValidator,
    UntypedValidator,
    mkTypedValidator,
    mkUntypedValidator,
    toBuiltinValidator,
  )
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as Validators
import PlutusTx qualified
import PlutusTx.Prelude

type StakingScript = TypedValidator StakingDatum StakingRedeemer

stakingScript :: StakingParams -> StakingScript
stakingScript params =
  mkTypedValidator
    (compiledValidateStaking `PlutusTx.applyCode` PlutusTx.liftCode params)

compiledValidateStaking ::
  PlutusTx.CompiledCode (StakingParams -> Validators.UntypedValidator)
compiledValidateStaking =
  optimizeUPLC $$(PlutusTx.compile [||toBuiltinValidator . validateStaking||])

alwaysPassingValidator :: UntypedValidator
alwaysPassingValidator =
  mkUntypedValidator
    $$(PlutusTx.compile [||(\_ _ _ -> check True)||])
