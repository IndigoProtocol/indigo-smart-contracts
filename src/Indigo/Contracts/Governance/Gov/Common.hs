-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Indigo.Contracts.Governance.Gov.Common
  ( ProtocolParams
      ( ProtocolParams,
        proposalDeposit,
        votingPeriod,
        effectiveDelay,
        expirationPeriod,
        protocolFeePercentage,
        totalShards,
        proposingPeriod
      ),
    ProposalContent
      ( ProposeAsset,
        MigrateAsset,
        ModifyProtocolParams,
        UpgradeProtocol,
        TextProposal
      ),
    GovParams (..),
    GovDatum
      ( Gov,
        currentProposal,
        currentVersion,
        protocolParams,
        protocolStartTime
      ),
    GovRedeemer (CreatePoll, UpgradeGov, UpgradeVersion),
    GovScript,
    UpgradePaths (UpgradePaths, uId, uPaths),
    UpgradePath (UpgradePath, upgradeSymbol),
    govManagerTokenName,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Generics (Generic)
import Indigo.Contracts.Oracle.Common (OracleAssetNFT)
import Indigo.Data.Decimal
import Ledger qualified
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Api (ValidatorHash)
import PlutusTx qualified
import PlutusTx.AssocMap (Map)
import PlutusTx.Builtins qualified as PB
import PlutusTx.Prelude
import Prelude qualified as P

govManagerTokenName :: Value.TokenName
govManagerTokenName = Value.TokenName "gov_nft"

data ProtocolParams = ProtocolParams
  { proposalDeposit :: Integer,
    votingPeriod :: Ledger.POSIXTime,
    effectiveDelay :: Ledger.POSIXTime,
    expirationPeriod :: Ledger.POSIXTime,
    protocolFeePercentage :: OnChainDecimal,
    -- time window for creating all voting shards
    proposingPeriod :: Ledger.POSIXTime,
    -- total numer of shards used for voting
    totalShards :: Integer
  }
  deriving stock (P.Show, Generic, P.Eq, P.Ord)
  deriving anyclass (ToJSON, FromJSON)

instance Eq ProtocolParams where
  {-# INLINEABLE (==) #-}
  (==) p1 p2 =
    proposalDeposit p1 == proposalDeposit p2
      && votingPeriod p1 == votingPeriod p2
      && effectiveDelay p1 == effectiveDelay p2
      && expirationPeriod p1 == expirationPeriod p2
      && proposingPeriod p1 == proposingPeriod p2
      && totalShards p1 == totalShards p2

PlutusTx.makeLift ''ProtocolParams
PlutusTx.makeIsDataIndexed ''ProtocolParams [('ProtocolParams, 0)]

-- | An upgrade proposal may need to contain more evidence than just the
-- CurrencySymbol, like the new validator. In that case the additional
-- evidence can be stored in new fields of the 'UpgradePath' data type.
newtype UpgradePath = UpgradePath {upgradeSymbol :: Value.CurrencySymbol}
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''UpgradePath
PlutusTx.makeIsDataIndexed ''UpgradePath [('UpgradePath, 0)]

data UpgradePaths = UpgradePaths
  { uId :: Integer,
    uPaths :: Map ValidatorHash UpgradePath
  }
  deriving (P.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''UpgradePaths
PlutusTx.makeIsDataIndexed ''UpgradePaths [('UpgradePaths, 0)]

data ProposalContent
  = ProposeAsset Value.TokenName OnChainDecimal OracleAssetNFT
  | MigrateAsset
      Value.TokenName
      OnChainDecimal
      (Either OnChainDecimal OracleAssetNFT)
  | ModifyProtocolParams ProtocolParams
  | UpgradeProtocol UpgradePaths
  | TextProposal PB.BuiltinByteString
  deriving (P.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''ProposalContent
PlutusTx.makeIsDataIndexed
  ''ProposalContent
  [ ('ProposeAsset, 0),
    ('MigrateAsset, 1),
    ('ModifyProtocolParams, 2),
    ('UpgradeProtocol, 3),
    ('TextProposal, 4)
  ]

{- Parameters of the Gov Script -}
data GovParams = GovParams
  { govNFT :: Value.AssetClass,
    pollToken :: Value.AssetClass,
    upgradeToken :: Value.AssetClass,
    indyAsset :: Value.AssetClass,
    versionRecordToken :: Value.AssetClass,
    pollManagerValHash :: ValidatorHash,
    gBiasTime :: Ledger.POSIXTime
  }
  deriving (Generic, P.Show, ToJSON, FromJSON, P.Eq, P.Ord, OpenApi.ToSchema)

PlutusTx.makeLift ''GovParams
PlutusTx.makeIsDataIndexed ''GovParams [('GovParams, 0)]

data GovDatum = Gov
  { -- | number of opened Proposal. Use for indexing Proposal
    currentProposal :: Integer,
    protocolParams :: ProtocolParams,
    -- | current version of the protocol, starting at 0
    currentVersion :: Integer,
    protocolStartTime :: Ledger.POSIXTime
  }
  deriving (Generic, P.Show, P.Eq, P.Ord)

PlutusTx.makeLift ''GovDatum
PlutusTx.makeIsDataIndexed ''GovDatum [('Gov, 0)]

data GovRedeemer
  = CreatePoll Ledger.POSIXTime Ledger.PaymentPubKeyHash ProposalContent
  | UpgradeGov
  | UpgradeVersion
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''GovRedeemer
PlutusTx.makeIsDataIndexed
  ''GovRedeemer
  [('CreatePoll, 0), ('UpgradeGov, 1), ('UpgradeVersion, 2)]

data GovScript

instance TScripts.ValidatorTypes GovScript where
  type DatumType GovScript = GovDatum
  type RedeemerType GovScript = GovRedeemer
