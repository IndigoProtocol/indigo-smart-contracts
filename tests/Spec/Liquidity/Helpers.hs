{-# LANGUAGE NamedFieldPuns #-}

module Spec.Liquidity.Helpers (findLiquidityPosition) where

import Indigo.Contracts.Liquidity.Common
  ( LiquidityDatum (LiquidityPosition, owner),
  )
import Indigo.Utils.Spooky qualified as Spooky
import Ledger qualified
import Plutus.Model (Run, TxBox (TxBox), txBoxDatum)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude
import Spec.Liquidity.Script (LiquidityScript, liquidityScript)
import Utils.Helpers (findUniqueUtxo)

findLiquidityPosition ::
  Ledger.PubKeyHash -> Run (V2.TxOutRef, V2.TxOut, LiquidityDatum)
findLiquidityPosition pk = findUniqueUtxo liquidityScript condition
  where
    condition :: TxBox LiquidityScript -> Bool
    condition TxBox {txBoxDatum = LiquidityPosition {owner}} = owner == spk
    spk :: Spooky.PubKeyHash
    spk = Spooky.toSpookyPubKeyHash pk
