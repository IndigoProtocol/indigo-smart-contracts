-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.Contracts.Treasury.Common
  ( TreasuryScriptParams (MkTreasuryScriptParams, versionRecordToken),
    TreasuryScript,
    TreasuryRedeemer (UpgradeVersion),
    daoIdentityTokenName,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Ledger.Orphans ()
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import PlutusTx qualified
import Prelude qualified as P

daoIdentityTokenName :: Value.TokenName
daoIdentityTokenName = "696E6469676F5F64616F5F746F6B656E" -- indigo_dao_token

data TreasuryScriptParams = MkTreasuryScriptParams
  { versionRecordToken :: Value.AssetClass
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord, OpenApi.ToSchema)

PlutusTx.makeLift ''TreasuryScriptParams
PlutusTx.makeIsDataIndexed ''TreasuryScriptParams [('MkTreasuryScriptParams, 0)]

data TreasuryRedeemer = UpgradeVersion
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''TreasuryRedeemer
PlutusTx.makeIsDataIndexed ''TreasuryRedeemer [('UpgradeVersion, 0)]

data TreasuryScript

instance TScripts.ValidatorTypes TreasuryScript where
  type DatumType TreasuryScript = ()
  type RedeemerType TreasuryScript = TreasuryRedeemer
