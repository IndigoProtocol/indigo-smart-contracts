{-# LANGUAGE DeriveAnyClass #-}

module Spec.StabilityPool.Params
  ( CreateParam (CreateParam, cTokenName, cAmount),
    DepositParam (DepositParam, dTokenName, dAmount),
    WithdrawParam (WithdrawParam, wTokenName, wAmount),
    CloseParam (CloseParam, clTokenName),
    RecordEpochToScaleToSumParam (RecordEpochToScaleToSumParam, ressTokenName),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Typed.Scripts qualified ()
import Ledger.Value qualified as Value
import PlutusTx.Prelude

data CreateParam = CreateParam
  { cTokenName :: Value.TokenName,
    cAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data DepositParam = DepositParam
  { dTokenName :: Value.TokenName,
    dAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data WithdrawParam = WithdrawParam
  { wTokenName :: Value.TokenName,
    wAmount :: Integer
  }
  deriving (Generic, ToJSON, FromJSON)

newtype CloseParam = CloseParam
  { clTokenName :: Value.TokenName
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RecordEpochToScaleToSumParam = RecordEpochToScaleToSumParam
  { ressTokenName :: Value.TokenName
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
