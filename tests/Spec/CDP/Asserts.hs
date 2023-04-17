{-# LANGUAGE NamedFieldPuns #-}

module Spec.CDP.Asserts (assertOpened, partialLiquidationAssert) where

import Control.Monad (unless)
import Indigo.Contracts.CDP.Common (CDPDatum (..))
import Indigo.Contracts.CDP.Common qualified as CDPParams
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Model (Run, TxBox (TxBox), logError, txBoxDatum, valueAt)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude hiding (unless)
import Spec.CDP.Helpers (cdpCreatorValue, iAssetAuthValue)
import Spec.CDP.Script (CDPScript, cdpCreatorScript, cdpScript)
import Utils.Helpers (findUniqueUtxo)
import Utils.Mock qualified as Mock
import Prelude (Int, replicate)

assertCDPScriptValue :: Integer -> Integer -> Int -> Run ()
assertCDPScriptValue cdpsCount ada oraclesCount = do
  val <- valueAt (cdpScript Mock.cdpParams)
  unless
    ( val
        == mconcat (replicate oraclesCount iAssetAuthValue)
          <> Value.assetClassValue
            (CDPParams.cdpAuthToken Mock.cdpParams)
            cdpsCount
          <> Ada.lovelaceValueOf ada
    )
    (logError "CDP script value is not as expected")

assertCDPCreatorScriptValue :: Run ()
assertCDPCreatorScriptValue = do
  val <- valueAt (cdpCreatorScript Mock.cdpCreatorParams)
  unless
    (val == cdpCreatorValue)
    (logError "CDP creator script value is not as expected")

assertOpened :: Integer -> Integer -> Int -> Run ()
assertOpened cdpsCount collateral oraclesCount =
  assertCDPScriptValue cdpsCount collateral oraclesCount
    >> assertCDPCreatorScriptValue

-- | When partial liquidation succeeds,
-- CDP UTXO should still exist.
partialLiquidationAssert :: Integer -> Value.TokenName -> Run ()
partialLiquidationAssert remainingCDPMintedAmt asset = do
  let condition :: TxBox CDPScript -> Bool
      condition
        TxBox
          { txBoxDatum =
              CDP
                { cdpOwner = Nothing,
                  cdpIAsset,
                  cdpMintedAmount
                }
          } =
          remainingCDPMintedAmt == cdpMintedAmount
            && cdpIAsset == asset
      condition _ = False
  (_, o, _) <-
    findUniqueUtxo
      (cdpScript Mock.cdpParams)
      condition
  unless
    ( Value.assetClassValueOf
        (V2.txOutValue o)
        (CDPParams.cdpAuthToken Mock.cdpParams)
        == 1
    )
    (logError "CDP output has correct amount of auth tokens")
