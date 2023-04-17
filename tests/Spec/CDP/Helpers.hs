{-# LANGUAGE NamedFieldPuns #-}

module Spec.CDP.Helpers
  ( iAssetAuthValue,
    cdpCreatorValue,
    openPositionSpendVal,
    findCDPCreator,
    findIAsset,
    findIAssetCustomNft,
    findCDP,
    findFrozenCDP,
  )
where

import GHC.Stack (HasCallStack)
import Indigo.Contracts.CDP.Common
  ( CDPDatum (CDP, IAssetDatum, cdpIAsset, cdpOwner),
    CDPScriptParams (cdpAuthToken),
    IAsset (IAsset, iaName),
  )
import Indigo.Contracts.CDP.Common qualified as CDPParams
import Indigo.Utils.Helpers (hasPositiveValue, hasUnitValue, unitValue)
import Ledger
import Ledger.Ada qualified as Ada
import Plutus.Model
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude hiding (pure)
import Spec.CDP.Params (OpenPositionParam (opCollateralAmount))
import Spec.CDP.Script
  ( CDPCreatorScript,
    CDPScript,
    cdpCreatorScript,
    cdpScript,
  )
import Utils.Helpers (StakingStrategy (..), findUniqueUtxo, minLovelacesPerUtxo, unwrapAppendStaking)
import Utils.Mock qualified as Mock
import Prelude (pure)

iAssetAuthValue :: Value
iAssetAuthValue =
  unitValue (CDPParams.iAssetAuthToken Mock.cdpParams)
    <> Ada.lovelaceValueOf minLovelacesPerUtxo

cdpCreatorValue :: Value
cdpCreatorValue =
  unitValue (CDPParams.cdpCreatorNft Mock.cdpCreatorParams)
    <> Ada.lovelaceValueOf minLovelacesPerUtxo

openPositionSpendVal :: OpenPositionParam -> Value
openPositionSpendVal opParams =
  Ada.lovelaceValueOf (opCollateralAmount opParams)
    <> Ada.lovelaceValueOf minLovelacesPerUtxo

findCDPCreator :: HasCallStack => Run (V2.TxOutRef, V2.TxOut)
findCDPCreator = do
  (oref, o, _) <-
    findUniqueUtxo (cdpCreatorScript Mock.cdpCreatorParams) condition
  pure (oref, o)
  where
    condition :: TxBox CDPCreatorScript -> Bool
    condition box =
      hasUnitValue
        (txBoxValue box)
        (CDPParams.cdpCreatorNft Mock.cdpCreatorParams)

findIAsset :: HasCallStack => TokenName -> Run (V2.TxOutRef, V2.TxOut, IAsset)
findIAsset name = do
  (oref, o, IAssetDatum ia) <-
    findUniqueUtxo (cdpScript Mock.cdpParams) condition
  pure (oref, o, ia)
  where
    condition :: TxBox CDPScript -> Bool
    condition box@TxBox {txBoxDatum = (IAssetDatum IAsset {iaName})} =
      iaName == name
        && hasUnitValue
          (txBoxValue box)
          (CDPParams.iAssetAuthToken Mock.cdpParams)
    condition _ = False

findIAssetCustomNft ::
  HasCallStack => TokenName -> AssetClass -> Run (V2.TxOutRef, V2.TxOut, IAsset)
findIAssetCustomNft name iAssetAuthNft = do
  (oref, o, IAssetDatum ia) <-
    findUniqueUtxo (cdpScript Mock.cdpParams) condition
  pure (oref, o, ia)
  where
    condition :: TxBox CDPScript -> Bool
    condition box@TxBox {txBoxDatum = (IAssetDatum IAsset {iaName})} =
      iaName == name && hasUnitValue (txBoxValue box) iAssetAuthNft
    condition _ = False

findCDP ::
  HasCallStack =>
  CDPScriptParams ->
  Ledger.PaymentPubKeyHash ->
  StakingStrategy ->
  TokenName ->
  Run (V2.TxOutRef, V2.TxOut, CDPDatum)
findCDP params pkh stakeStrategy tn = do
  case stakeStrategy of
    IgnoreStaking ->
      findUniqueUtxo (cdpScript Mock.cdpParams) condition
    UseStakingKey pkh' ->
      findUniqueUtxo
        (appendStakingPubKey pkh' $ cdpScript Mock.cdpParams)
        (condition . unwrapAppendStaking)
  where
    condition :: TxBox CDPScript -> Bool
    condition box@TxBox {txBoxDatum = CDP {cdpOwner, cdpIAsset}} =
      cdpOwner == Just pkh
        && cdpIAsset == tn
        && hasUnitValue (txBoxValue box) (cdpAuthToken params)
    condition _ = False

findFrozenCDP ::
  HasCallStack =>
  CDPScriptParams ->
  TokenName ->
  Run (V2.TxOutRef, V2.TxOut, CDPDatum)
findFrozenCDP params tn = findUniqueUtxo (cdpScript Mock.cdpParams) condition
  where
    condition :: TxBox CDPScript -> Bool
    condition box@TxBox {txBoxDatum = CDP {cdpOwner, cdpIAsset}} =
      isNothing cdpOwner
        && cdpIAsset == tn
        && hasPositiveValue (cdpAuthToken params) (txBoxValue box)
    condition _ = False
