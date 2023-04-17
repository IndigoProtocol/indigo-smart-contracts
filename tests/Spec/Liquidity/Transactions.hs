{-# LANGUAGE NamedFieldPuns #-}

module Spec.Liquidity.Transactions
  ( UpdateLiquidityPositionVariation (UpdateSucceed, UpdateSignedByOtherUser),
    CloseLiquidityPositionVariation (CloseSucceed, CloseSignedByOtherUser),
    runCreateLiquidityPosition,
    runUpdateLiquidityPosition,
    runCloseLiquidityPosition,
    runInitialize,
  )
where

import Control.Monad (unless, void)
import Indigo.Contracts.Liquidity.Common
  ( LiquidityDatum (LiquidityPosition, owner),
  )
import Indigo.Utils.Spooky qualified as Spooky
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Model
  ( Run,
    checkBalanceBy,
    gives,
    logError,
    newUser,
    noErrors,
    payToKey,
    payToRef,
    sendTx,
    signTx,
    spendScriptRef,
    userSpend,
    withSpend,
  )
import Plutus.Model.Contract (DatumMode (InlineDatum))
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude hiding (mconcat, mempty, pure, unless)
import Spec.Liquidity.Helpers (findLiquidityPosition)
import Spec.Liquidity.Params
  ( CloseLiquidityPositionParam,
    CreateLiquidityPositionParam
      ( CreateLiquidityPositionParam,
        cTokenAmountPairs
      ),
    UpdateLiquidityPositionParam
      ( UpdateLiquidityPositionParam,
        uTokenAmountPairs
      ),
  )
import Spec.Liquidity.Script (liquidityScript)
import Utils.Helpers (getFirstRefScript, initScriptRef)
import Prelude (mconcat, mempty, pure)

data UpdateLiquidityPositionVariation = UpdateSucceed | UpdateSignedByOtherUser

data CloseLiquidityPositionVariation = CloseSucceed | CloseSignedByOtherUser

-- place this after `runCreateLiquidityPosition` to
-- work around the checkBalanceBy failure
runInitialize :: Run ()
runInitialize = initScriptRef liquidityScript

runCreateLiquidityPosition ::
  Ledger.PubKeyHash -> CreateLiquidityPositionParam -> Run ()
runCreateLiquidityPosition
  user
  CreateLiquidityPositionParam {cTokenAmountPairs} = do
    unless (all (> 0) $ fmap snd cTokenAmountPairs) $
      logError "Staking amount cannot be negative or zero"
    let stakeValue = foldMap (uncurry Value.assetClassValue) cTokenAmountPairs
    void $
      checkBalanceBy
        ( \success ->
            if success
              then gives user stakeValue liquidityScript
              else mempty
        )
        ( do
            withSpend
              user
              stakeValue
              ( \sp -> do
                  let tx =
                        mconcat
                          [ userSpend sp,
                            payToRef
                              liquidityScript
                              ( InlineDatum $
                                  LiquidityPosition
                                    { owner = Spooky.toSpookyPubKeyHash user
                                    }
                              )
                              stakeValue
                          ]
                  void $ signTx user tx >>= sendTx
              )
            noErrors
        )

runUpdateLiquidityPosition ::
  Ledger.PubKeyHash ->
  UpdateLiquidityPositionParam ->
  UpdateLiquidityPositionVariation ->
  Run ()
runUpdateLiquidityPosition
  user
  UpdateLiquidityPositionParam {uTokenAmountPairs}
  variation = do
    refScriptUtxo <- getFirstRefScript liquidityScript
    (orefLp, oLp, lqPositionDatum) <- findLiquidityPosition user
    signatory <- case variation of
      UpdateSignedByOtherUser -> newUser mempty
      _ -> pure user
    let valueDelta = foldMap (uncurry Value.assetClassValue) uTokenAmountPairs
    withSpend
      user
      valueDelta
      ( \sp -> do
          let tx =
                mconcat
                  [ userSpend sp,
                    spendScriptRef
                      refScriptUtxo
                      liquidityScript
                      orefLp
                      ()
                      lqPositionDatum,
                    payToRef
                      liquidityScript
                      (InlineDatum lqPositionDatum)
                      (V2.txOutValue oLp <> valueDelta)
                  ]
          void $ signTx signatory tx >>= sendTx
      )

runCloseLiquidityPosition ::
  Ledger.PubKeyHash ->
  CloseLiquidityPositionParam ->
  CloseLiquidityPositionVariation ->
  Run ()
runCloseLiquidityPosition user _ variation = do
  refScriptUtxo <- getFirstRefScript liquidityScript
  (orefLp, oLp, lqPositionDatum) <- findLiquidityPosition user
  signatory <- case variation of
    CloseSignedByOtherUser -> newUser mempty
    _ -> pure user
  let tx =
        mconcat
          [ spendScriptRef
              refScriptUtxo
              liquidityScript
              orefLp
              ()
              lqPositionDatum,
            payToKey user (V2.txOutValue oLp)
          ]
  void $ signTx signatory tx >>= sendTx
