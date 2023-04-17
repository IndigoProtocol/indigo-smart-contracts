module Spec.Oracle.Transactions
  ( startOracle,
    feedPrice,
    useOracleTx,
    useOracleTxCustomNft,
    runInitRefScript,
    FeedPriceVariation (..),
  )
where

import Control.Monad (unless, void)
import Indigo.Contracts.Oracle.Common
  ( OracleAssetNFT (MkOracleAssetNFT),
    OracleDatum (MkOracleDatum, odExpiration, odPrice),
    OracleParams (opBiasTime, opExpirationTime),
    OracleRedeemer (FeedPrice),
  )
import Indigo.Data.Decimal
import Indigo.Utils.Helpers (oneSecond, oneYear)
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Model
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude hiding (fmap, mconcat, pure, unless, (*))
import Spec.Oracle.Helpers
  ( findOracle,
    findOracleCustomNft,
    oAssetNFTAssetClass,
    oracleValue,
  )
import Spec.Oracle.Script (oracleScript)
import Utils.Helpers (getFirstRefScript, initScriptRef)
import Utils.Mock qualified as Mock
import Prelude (fmap, mconcat, pure, (*))

data FeedPriceVariation
  = FeedPriceSucceed
  | ExpirationInPast
  | ExpirationInFarFuture

runInitRefScript :: Run ()
runInitRefScript = do
  param <- Mock.oracleParams
  initScriptRef (oracleScript param)

startOracle :: Ledger.TokenName -> OnChainDecimal -> Run OracleAssetNFT
startOracle token price = do
  admin <- getMainUser
  param <- Mock.oracleParams
  runInitRefScript
  sp <- spend admin (oracleValue token)
  void $ startOracleTx token price sp >>= signTx admin >>= sendTx
  oracleVal <- valueAt (oracleScript param)
  unless
    (Value.assetClassValueOf oracleVal (oAssetNFTAssetClass token) == 1)
    (logError "Oracle does not have nft")
  pure (MkOracleAssetNFT $ oAssetNFTAssetClass token)

startOracleTx :: Ledger.TokenName -> OnChainDecimal -> UserSpend -> Run Tx
startOracleTx token price sp = do
  param <- Mock.oracleParams
  oracleExpiration <- fmap (+ opExpirationTime param) currentTime
  pure $
    mconcat
      [ userSpend sp,
        payToRef
          (oracleScript param)
          ( InlineDatum $
              MkOracleDatum {odPrice = price, odExpiration = oracleExpiration}
          )
          (oracleValue token)
      ]

useOracleTx :: Ledger.TokenName -> Run Tx
useOracleTx token = do
  (oref, _, _) <- findOracle token
  now <- currentTime
  validateIn
    (Ledger.to (now + 20 * oneSecond))
    ( mconcat
        [ refInputInline oref
        ]
    )

useOracleTxCustomNft :: Ledger.AssetClass -> Run Tx
useOracleTxCustomNft oracleNft = do
  (oref, _, _) <- findOracleCustomNft oracleNft
  now <- currentTime
  validateIn
    (Ledger.to (now + 20 * oneSecond))
    ( mconcat
        [ refInputInline oref
        ]
    )

feedPrice :: Ledger.TokenName -> OnChainDecimal -> FeedPriceVariation -> Run ()
feedPrice token price variation = do
  admin <- getMainUser
  param <- Mock.oracleParams
  (oref, o, datum) <- findOracle token
  now <- case variation of
    ExpirationInFarFuture -> fmap (+ (oneYear * 10)) currentTime
    _ -> currentTime
  refScriptUtxo <- getFirstRefScript (oracleScript param)
  tx <-
    validateIn
      ( case variation of
          ExpirationInFarFuture ->
            Ledger.to (now + opBiasTime param - oneSecond)
          _ ->
            Ledger.interval
              (now - opBiasTime param + oneSecond)
              (now + opBiasTime param - oneSecond)
      )
      ( mconcat
          [ spendScriptRef
              refScriptUtxo
              (oracleScript param)
              oref
              (FeedPrice now price)
              datum,
            payToRef
              (oracleScript param)
              ( InlineDatum $
                  MkOracleDatum
                    { odPrice = price,
                      odExpiration =
                        oracleExpiration now (opExpirationTime param)
                    }
              )
              (V2.txOutValue o)
          ]
      )
  void $ signTx admin tx >>= sendTx
  where
    oracleExpiration :: V2.POSIXTime -> V2.POSIXTime -> V2.POSIXTime
    oracleExpiration now expir = case variation of
      ExpirationInPast -> now - expir
      _ -> now + expir
