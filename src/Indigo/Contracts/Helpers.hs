-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE NamedFieldPuns #-}

module Indigo.Contracts.Helpers
  ( getProtocolFeePercentage,
    payProtocolFeeCorrectly,
  )
where

import Indigo.Contracts.Governance.Gov.Common qualified as Gov
import Indigo.Data.Decimal (OnChainDecimal (..))
import Indigo.Utils.Helpers qualified as Helpers
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude

{-# INLINEABLE getProtocolFeePercentage #-}
getProtocolFeePercentage :: Value.AssetClass -> V2.TxInfo -> OnChainDecimal
getProtocolFeePercentage govNft info = protocolFeePercentage
  where
    govInput :: V2.TxOut
    govInput = Helpers.findUniqueReferenceInputWithToken govNft info

    Gov.Gov {Gov.protocolParams} = Helpers.findInlinedDatumFromOutput govInput

    Gov.ProtocolParams {Gov.protocolFeePercentage} = protocolParams

{-# INLINEABLE payProtocolFeeCorrectly #-}
payProtocolFeeCorrectly :: V2.ValidatorHash -> V2.TxInfo -> Integer -> Bool
payProtocolFeeCorrectly collectorValHash info fee =
  fee == zero
    || Helpers.checkOutputFromOtherScripts
      info
      collectorValHash
      ()
      (feeInputValue <> Ada.lovelaceValueOf fee)
  where
    feeInput :: V2.TxOut
    feeInput = case Helpers.findAllInputsFromScript collectorValHash info of
      [o] -> o
      _ -> traceError "Must spend 1 input from collector script"

    feeInputValue :: Value.Value
    feeInputValue = V2.txOutValue feeInput
