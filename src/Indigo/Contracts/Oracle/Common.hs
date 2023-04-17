-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-
Common functions, data structures for Oracle script.
-}

module Indigo.Contracts.Oracle.Common
  ( oracleAssetNFTTokenName,
    OracleAssetNFT (MkOracleAssetNFT),
    OracleParams (OracleParams, opOwner, opBiasTime, opExpirationTime),
    OracleDatum (MkOracleDatum, odPrice, odExpiration),
    OracleRedeemer (FeedPrice),
    Oracle,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Indigo.Data.Decimal (OnChainDecimal)
import Indigo.Utils.Spooky qualified as Spooky
import Ledger (POSIXTime)
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import PlutusTx qualified
import Prelude qualified as P

{-# INLINEABLE oracleAssetNFTTokenName #-}
oracleAssetNFTTokenName :: Value.TokenName
oracleAssetNFTTokenName = Value.TokenName "oracle_asset_nft"

-- | The type for the NFT that represents oracle asset (e.g. iBTC asset)
newtype OracleAssetNFT = MkOracleAssetNFT Value.AssetClass
  deriving stock (P.Show, P.Ord, P.Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''OracleAssetNFT
PlutusTx.makeIsDataIndexed ''OracleAssetNFT [('MkOracleAssetNFT, 0)]

data OracleParams = OracleParams
  { -- | Owner of the Oracle - the PubKeyHash that can
    -- update the authentic Oracle output with NFT
    opOwner :: Spooky.PubKeyHash,
    -- | the time offset by which a time approximation is considered valid
    opBiasTime :: POSIXTime,
    -- | the time how long the oracle is active
    opExpirationTime :: POSIXTime
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord, OpenApi.ToSchema)

PlutusTx.makeLift ''OracleParams
PlutusTx.makeIsDataIndexed ''OracleParams [('OracleParams, 0)]

data OracleDatum = MkOracleDatum
  { -- | Price in lovelace with decimal places
    odPrice :: OnChainDecimal,
    odExpiration :: POSIXTime
  }
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''OracleDatum
PlutusTx.makeIsDataIndexed ''OracleDatum [('MkOracleDatum, 0)]

data OracleRedeemer
  = -- | The first parameter is the current time
    FeedPrice POSIXTime OnChainDecimal
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''OracleRedeemer
PlutusTx.makeIsDataIndexed ''OracleRedeemer [('FeedPrice, 0)]

data Oracle

instance TScripts.ValidatorTypes Oracle where
  type DatumType Oracle = OracleDatum
  type RedeemerType Oracle = OracleRedeemer
