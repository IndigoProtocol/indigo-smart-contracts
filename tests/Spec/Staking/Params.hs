{-# LANGUAGE DeriveAnyClass #-}

module Spec.Staking.Params
  ( StartStakingParam
      ( StartStakingParam,
        indyAssetClass,
        pollToken,
        cdpToken,
        versionRecordToken
      ),
    OpenStakingPositionParam (OpenStakingPositionParam, oAmount),
    StakeParam (StakeParam, sAmount),
    WithdrawStakeParam (WithdrawStakeParam, wAmount),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Typed.Scripts qualified ()
import Ledger.Value qualified as Value
import PlutusTx.Prelude

data StartStakingParam = StartStakingParam
  { indyAssetClass :: Value.AssetClass,
    pollToken :: Value.AssetClass,
    cdpToken :: Value.AssetClass,
    versionRecordToken :: Value.AssetClass
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OpenStakingPositionParam = OpenStakingPositionParam
  { oAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype StakeParam = StakeParam
  { sAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WithdrawStakeParam = WithdrawStakeParam
  { wAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
