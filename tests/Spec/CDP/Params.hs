{-# LANGUAGE DeriveAnyClass #-}

module Spec.CDP.Params
  ( OpenPositionParam
      ( OpenPositionParam,
        opAsset,
        opCollateralAmount,
        opMintedAmount
      ),
    DepositParam (DepositParam, dCdpOwnerPkh, dAsset, dAmount),
    WithdrawParam (WithdrawParam, wCdpOwnerPkh, wAsset, wAmount),
    MintParam (MintParam, mCdpOwnerPkh, mAsset, mAmount),
    BurnParam (BurnParam, bCdpOwnerPkh, bAsset, bAmount),
    CloseParam (CloseParam, cCdpOwnerPkh, cAsset),
    LiquidateParam (LiquidateParam, lAsset),
    FreezeParam (FreezeParam, fCdpOwnerPkh, fAsset),
    MergeCDPsParam (MergeCDPsParam, cdpsToMerge, mpAsset),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude

data OpenPositionParam = OpenPositionParam
  { opAsset :: Value.TokenName,
    opCollateralAmount :: Integer,
    opMintedAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data DepositParam = DepositParam
  { dCdpOwnerPkh :: Ledger.PaymentPubKeyHash,
    dAsset :: Value.TokenName,
    dAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data WithdrawParam = WithdrawParam
  { wCdpOwnerPkh :: Ledger.PaymentPubKeyHash,
    wAsset :: Value.TokenName,
    wAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data MintParam = MintParam
  { mCdpOwnerPkh :: Ledger.PaymentPubKeyHash,
    mAsset :: Value.TokenName,
    mAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data BurnParam = BurnParam
  { bCdpOwnerPkh :: Ledger.PaymentPubKeyHash,
    bAsset :: Value.TokenName,
    bAmount :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data CloseParam = CloseParam
  { cCdpOwnerPkh :: Ledger.PaymentPubKeyHash,
    cAsset :: Value.TokenName
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data FreezeParam = FreezeParam
  { fCdpOwnerPkh :: Ledger.PubKeyHash,
    fAsset :: Value.TokenName
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data LiquidateParam = LiquidateParam
  { lAsset :: Value.TokenName
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data MergeCDPsParam = MergeCDPsParam
  { cdpsToMerge :: [V2.TxOutRef],
    mpAsset :: Value.TokenName
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
