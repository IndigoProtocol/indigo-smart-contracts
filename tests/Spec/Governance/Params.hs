{-# LANGUAGE DeriveAnyClass #-}

module Spec.Governance.Params
  ( CreateProposalParam (CreateProposalParam, cpContent),
    VoteParam (VoteParam, vId, vOption),
    EndParam (EndParam, eId, eTotalShards),
    ExecuteParam (ExecuteParam, exId),
    CreateShardsParam (CreateShardsParam, csId, csEndTime, csTotalShards),
    MergeShardsParam (MergeShardsParam, msId),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Indigo.Contracts.Governance.Gov.Common qualified as Gov
import Indigo.Contracts.Governance.Poll.Common qualified as Poll
import Ledger qualified
import PlutusTx.Prelude

data CreateProposalParam = CreateProposalParam
  { cpContent :: Gov.ProposalContent
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data VoteParam = VoteParam
  { vId :: Integer,
    vOption :: Poll.VoteOption
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data CreateShardsParam = CreateShardsParam
  { csId :: Integer,
    csEndTime :: Ledger.POSIXTime,
    csTotalShards :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MergeShardsParam = MergeShardsParam
  { msId :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data EndParam = EndParam
  { eId :: Integer,
    -- required for plutus-simple-model tests
    -- in order to know how much extra ada you get after merging all shards
    eTotalShards :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ExecuteParam = ExecuteParam {exId :: Integer}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
