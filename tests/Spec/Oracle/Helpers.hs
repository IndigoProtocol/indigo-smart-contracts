{-# LANGUAGE NamedFieldPuns #-}

module Spec.Oracle.Helpers
  ( oracleValue,
    oAssetNFTAssetClass,
    findPrice,
    findOracle,
    findOracleCustomNft,
    waitUntilOracleExpired,
  )
where

import GHC.Stack (HasCallStack)
import Indigo.Contracts.Oracle.Common
  ( OracleAssetNFT,
    OracleDatum (MkOracleDatum, odPrice),
    OracleParams (opExpirationTime),
  )
import Indigo.Data.Decimal
import Indigo.Utils.Helpers (hasUnitValue, unitValue)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Value
import Plutus.Model
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude hiding (mempty, pure)
import Spec.Oracle.Script (OracleScript, oracleScript)
import Utils.Helpers (findUniqueUtxo, minLovelacesPerUtxo)
import Utils.Mock qualified as Mock
import Prelude (mempty, pure)

oracleValue :: TokenName -> Value
oracleValue token =
  Ada.lovelaceValueOf minLovelacesPerUtxo
    <> unitValue (oAssetNFTAssetClass token)

oAssetNFTAssetClass :: Ledger.TokenName -> Ledger.AssetClass
oAssetNFTAssetClass (TokenName tag) = fakeCoin (FakeCoin tag)

findPrice ::
  Either OnChainDecimal OracleAssetNFT ->
  (TokenName -> Run Tx) ->
  Ledger.TokenName ->
  Run (Tx, OnChainDecimal)
findPrice (Left finalPrice) _ _ = pure (mempty, finalPrice)
findPrice (Right _) useOracle token = do
  (_, _, MkOracleDatum {odPrice}) <- findOracle token
  tx <- useOracle token
  return (tx, odPrice)

findOracle ::
  HasCallStack =>
  Ledger.TokenName ->
  Run (V2.TxOutRef, V2.TxOut, OracleDatum)
findOracle token = do
  param <- Mock.oracleParams
  findUniqueUtxo (oracleScript param) condition
  where
    condition :: TxBox OracleScript -> Bool
    condition box = hasUnitValue (txBoxValue box) (oAssetNFTAssetClass token)

findOracleCustomNft ::
  HasCallStack => Ledger.AssetClass -> Run (V2.TxOutRef, V2.TxOut, OracleDatum)
findOracleCustomNft oracleNft = do
  param <- Mock.oracleParams
  findUniqueUtxo (oracleScript param) condition
  where
    condition :: TxBox OracleScript -> Bool
    condition box = hasUnitValue (txBoxValue box) oracleNft

waitUntilOracleExpired :: Run ()
waitUntilOracleExpired = do
  oParams <- Mock.oracleParams
  wait (opExpirationTime oParams)
