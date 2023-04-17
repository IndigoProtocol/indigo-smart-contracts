{-# LANGUAGE TemplateHaskell #-}

module Spec.Liquidity.Script (LiquidityScript, liquidityScript) where

import Indigo.Contracts.Liquidity.Common (LiquidityDatum)
import Indigo.Contracts.Liquidity.OnChain (validateLiquidity)
import Indigo.Utils.Helpers (optimizeUPLC)
import Indigo.Utils.Spooky qualified as Spooky
import Plutus.Model.V2 (TypedValidator, mkTypedValidator)
import PlutusTx qualified
import PlutusTx.Prelude

type LiquidityScript = TypedValidator LiquidityDatum ()

liquidityScript :: LiquidityScript
liquidityScript =
  mkTypedValidator $
    optimizeUPLC
      $$(PlutusTx.compile [||Spooky.mkUntypedValidator validateLiquidity||])
