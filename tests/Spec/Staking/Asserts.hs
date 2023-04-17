{-# LANGUAGE NamedFieldPuns #-}

module Spec.Staking.Asserts (assertLengthVoteMap) where

import Control.Monad (unless)
import Indigo.Contracts.Staking.Common
  ( StakingDatum (StakingPosition, lockedAmount),
  )
import Ledger qualified
import Plutus.Model (Run, logError)
import PlutusTx.AssocMap qualified as PtMap
import PlutusTx.Prelude hiding (unless)
import Spec.Staking.Helpers (findStakingPosition)
import Prelude qualified as P

assertLengthVoteMap :: Ledger.PubKeyHash -> P.Int -> Run ()
assertLengthVoteMap user n = do
  (_, _, StakingPosition {lockedAmount}) <-
    findStakingPosition (Ledger.PaymentPubKeyHash user)
  let l = P.length (PtMap.toList lockedAmount)
  unless
    (l P.== n)
    (logError $ "Unexpected map size: " <> P.show l)
