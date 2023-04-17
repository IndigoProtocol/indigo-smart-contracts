-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Indigo.Contracts.Liquidity.OnChain
  ( validateLiquidity,
    liquidityValidator,
    liquidityAddress,
    liquidityScriptCTL,
    untypedLiquidityValidatorHash,
  )
where

import Indigo.Contracts.Liquidity.Common
import Indigo.Utils.Helpers qualified as Helpers
import Indigo.Utils.Spooky qualified as Spooky
import Ledger qualified
import Plutus.Script.Utils.V2.Address qualified as Address
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude hiding (divide)

{-# INLINEABLE validateLiquidity #-}
validateLiquidity :: LiquidityDatum -> () -> Spooky.ScriptContext -> Bool
validateLiquidity LiquidityPosition {owner} _ ctx =
  traceIfFalse "Must be signed by owner" (Spooky.txSignedBy info owner)
  where
    info :: Spooky.TxInfo
    info = Spooky.scriptContextTxInfo ctx

liquidityValidator :: V2.Validator
liquidityValidator =
  V2.mkValidatorScript compiledUntypedValidateLiquidity

compiledUntypedValidateLiquidity :: PlutusTx.CompiledCode UntypedValidator
compiledUntypedValidateLiquidity =
  Helpers.optimizeUPLC $$(PlutusTx.compile [||wrap||])
  where
    wrap = Spooky.mkUntypedValidator validateLiquidity

untypedLiquidityValidator :: V2.Validator
untypedLiquidityValidator =
  V2.mkValidatorScript compiledUntypedValidateLiquidity

untypedLiquidityValidatorHash :: V2.ValidatorHash
untypedLiquidityValidatorHash = Scripts.validatorHash untypedLiquidityValidator

-- serialised for ue in CTL
liquidityScriptCTL :: V2.Script
liquidityScriptCTL = V2.fromCompiledCode compiledUntypedValidateLiquidity

liquidityAddress :: Ledger.Address
liquidityAddress = Address.mkValidatorAddress liquidityValidator
