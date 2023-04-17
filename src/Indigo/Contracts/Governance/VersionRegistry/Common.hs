-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.Contracts.Governance.VersionRegistry.Common
  ( VersionRecord (VersionRecord, versionId, versionPaths),
    VersionRecordParams (VersionRecordParams, upgradeToken),
    VersionRecordRedeemer,
    VersionRecordScript,
    VersionRecordMintingPolicyRedeemer (AddRecord),
    validateUpgradeVersion,
    validateUpgradeVersion',
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as OpenApi
import Data.Void (Void)
import GHC.Generics (Generic)
import Indigo.Utils.Helpers qualified as Helpers
import Indigo.Utils.Spooky qualified as Spooky
import Indigo.Utils.Spooky.Helpers qualified as Spooky.Helpers
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (ValidatorHash)
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude
import Prelude qualified as P

data VersionRecord = VersionRecord
  { versionId :: Integer,
    versionPaths :: Map ValidatorHash Value.CurrencySymbol
  }
  deriving (Generic, P.Show, P.Eq, ToJSON, FromJSON)

-- deriveEq ''VersionRecord
PlutusTx.makeLift ''VersionRecord
PlutusTx.makeIsDataIndexed ''VersionRecord [('VersionRecord, 0)]

-- TODO: use `deriveEq ''VersionRecord` of
-- https://github.com/Liqwid-Labs/plutus-extra
instance Eq VersionRecord where
  a == b =
    versionId a == versionId b
      && versionPaths a == versionPaths b

data VersionRecordParams = VersionRecordParams
  { upgradeToken :: Value.AssetClass
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord, OpenApi.ToSchema)

PlutusTx.makeLift ''VersionRecordParams
PlutusTx.makeIsDataIndexed ''VersionRecordParams [('VersionRecordParams, 0)]

type VersionRecordRedeemer = Void

data VersionRecordMintingPolicyRedeemer = AddRecord
  deriving (Generic, P.Show, P.Eq, ToJSON, FromJSON)

PlutusTx.makeLift ''VersionRecordMintingPolicyRedeemer
PlutusTx.makeIsDataIndexed
  ''VersionRecordMintingPolicyRedeemer
  [('AddRecord, 0)]

data VersionRecordScript

instance TScripts.ValidatorTypes VersionRecordScript where
  type DatumType VersionRecordScript = VersionRecord
  type RedeemerType VersionRecordScript = VersionRecordRedeemer

-- | Utility function used by various UpgradeVersion redeemers
{-# INLINEABLE validateUpgradeVersion #-}
validateUpgradeVersion ::
  BuiltinString ->
  V2.ScriptContext ->
  Value.AssetClass ->
  Bool
validateUpgradeVersion scriptName ctx versionRecordToken
  | Just ownUpgrade <- AssocMap.lookup (Contexts.ownHash ctx) versionPaths =
      traceIfFalse
        ("Missing " <> scriptName <> " upgrade minting policy")
        (ownUpgrade `AssocMap.member` Value.getValue (V2.txInfoMint info))
  | otherwise = trace ("Missing " <> scriptName <> " upgrade path") False
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx
    versionRecordInput :: V2.TxOut
    versionRecordInput =
      Helpers.findUniqueReferenceInputWithToken versionRecordToken info
    versionPaths :: AssocMap.Map ValidatorHash Value.CurrencySymbol
    VersionRecord {versionPaths} =
      Helpers.findInlinedDatumFromOutput versionRecordInput

-- | Utility function used by various Spooky UpgradeVersion redeemers
{-# INLINEABLE validateUpgradeVersion' #-}
validateUpgradeVersion' ::
  BuiltinString ->
  Spooky.ScriptContext ->
  Spooky.AssetClass ->
  Bool
validateUpgradeVersion' scriptName ctx versionRecordToken
  | Just ownUpgrade <-
      AssocMap.lookup
        (Spooky.unSpookyValidatorHash $ Spooky.ownHash ctx)
        versionPaths =
      traceIfFalse
        ("Missing " <> scriptName <> " upgrade minting policy")
        ( Spooky.toSpookyCurrencySymbol ownUpgrade
            `AssocMap.member` Spooky.getValue (Spooky.txInfoMint info)
        )
  | otherwise = trace ("Missing " <> scriptName <> " upgrade path") False
  where
    info :: Spooky.TxInfo
    info = Spooky.scriptContextTxInfo ctx
    versionRecordInput :: Spooky.TxOut
    versionRecordInput =
      Spooky.Helpers.findUniqueReferenceInputWithToken versionRecordToken info
    versionPaths :: AssocMap.Map ValidatorHash Value.CurrencySymbol
    VersionRecord {versionPaths} =
      Spooky.Helpers.findInlinedDatumFromOutput versionRecordInput
