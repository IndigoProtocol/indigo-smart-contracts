-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.Contracts.Liquidity.Common
  ( LiquidityDatum (LiquidityPosition, owner),
    Liquidity,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Indigo.Utils.Spooky qualified as Spooky
import Ledger.Typed.Scripts qualified as TScripts
import PlutusTx qualified
import Prelude qualified as P

-- | Represents LP Tokens staking account/profile of an individual PubKeyHash.
data LiquidityDatum = LiquidityPosition
  { owner :: Spooky.PubKeyHash
  }
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''LiquidityDatum
PlutusTx.makeIsDataIndexed ''LiquidityDatum [('LiquidityPosition, 0)]

data Liquidity

instance TScripts.ValidatorTypes Liquidity where
  type DatumType Liquidity = LiquidityDatum
  type RedeemerType Liquidity = ()
