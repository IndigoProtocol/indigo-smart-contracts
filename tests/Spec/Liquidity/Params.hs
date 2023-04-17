{-# LANGUAGE DeriveAnyClass #-}

module Spec.Liquidity.Params
  ( CreateLiquidityPositionParam
      ( CreateLiquidityPositionParam,
        cTokenAmountPairs
      ),
    UpdateLiquidityPositionParam
      ( UpdateLiquidityPositionParam,
        uTokenAmountPairs
      ),
    CloseLiquidityPositionParam (CloseLiquidityPositionParam),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Typed.Scripts qualified ()
import Ledger.Value qualified as Value
import PlutusTx.Prelude

data CreateLiquidityPositionParam = CreateLiquidityPositionParam
  { cTokenAmountPairs :: [(Value.AssetClass, Integer)]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data UpdateLiquidityPositionParam = UpdateLiquidityPositionParam
  { uTokenAmountPairs :: [(Value.AssetClass, Integer)]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data CloseLiquidityPositionParam = CloseLiquidityPositionParam
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
