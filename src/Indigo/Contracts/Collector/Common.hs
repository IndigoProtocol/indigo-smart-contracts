-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-
Common functions, data structures for CDP Script.
-}

module Indigo.Contracts.Collector.Common
  ( CollectorScriptParams
      ( CollectorScriptParams,
        stakingManagerNFT,
        stakingToken,
        versionRecordToken
      ),
    CollectorRedeemer (Collect, UpgradeVersion),
    CollectorScript,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Indigo.Utils.Spooky qualified as Spooky
import Ledger.Orphans ()
import Ledger.Typed.Scripts qualified as TScripts
import PlutusTx qualified
import Prelude qualified as P

{-
Parameters of Collector Script.
-}
data CollectorScriptParams = CollectorScriptParams
  { stakingManagerNFT :: Spooky.AssetClass,
    stakingToken :: Spooky.AssetClass,
    versionRecordToken :: Spooky.AssetClass
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord, OpenApi.ToSchema)

PlutusTx.makeLift ''CollectorScriptParams
PlutusTx.makeIsDataIndexed ''CollectorScriptParams [('CollectorScriptParams, 0)]

data CollectorRedeemer = Collect | UpgradeVersion
  deriving stock (P.Eq, P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''CollectorRedeemer
PlutusTx.makeIsDataIndexed
  ''CollectorRedeemer
  [('Collect, 0), ('UpgradeVersion, 1)]

data CollectorScript

instance TScripts.ValidatorTypes CollectorScript where
  type DatumType CollectorScript = ()
  type RedeemerType CollectorScript = CollectorRedeemer
