{-# LANGUAGE NamedFieldPuns #-}

module Spec.StabilityPool.Transactions
  ( RecordVariation (RecordSucceed, StealAccountToken),
    OpenVariation (OpenSucceed, ModifyPoolStakingCredential),
    runInitStabilityPool,
    runCreate,
    runDeposit,
    runClose,
    runWithdraw,
    runRecordEpochToScaleToSum,
    runSpendAccountWithSP,
    runInitialize,
    runInitRefScript,
  )
where

import Control.Lens (over, view, _1)
import Control.Monad (void, when)
import GHC.Stack (HasCallStack)
import Indigo.Contracts.CDP.Common (cdpAssetSymbol, protocolFee)
import Indigo.Contracts.Governance.Gov.Common qualified as GovParams
import Indigo.Contracts.StabilityPool.Common
  ( StabilityDatum
      ( AccountDatum,
        SnapshotEpochToScaleToSumDatum,
        StabilityPoolDatum,
        epochToScaleToSum,
        sessAsset,
        sessSnapshot,
        spIAsset,
        spSnapshot
      ),
    StabilityPoolParams
      ( accountAdjustmentFeeLovelaces,
        accountCreateFeeLovelaces,
        accountToken,
        assetSymbol,
        snapshotEpochToScaleToSumToken,
        stabilityPoolToken
      ),
    StabilityPoolRedeemer
      ( AdjustAccount,
        Close,
        CreateAccount,
        RecordEpochToScaleToSum,
        SpendAccount
      ),
    StabilityPoolSnapshot
      ( StabilityPoolSnapshot,
        snapshotEpoch,
        snapshotP,
        snapshotS,
        snapshotScale
      ),
    aaDepositChange,
    fromSPInteger,
    initEpochToScaleToSumMap,
    initSPSnapshot,
    partitionEpochToScaleToSumMap,
    snapshotD,
    snapshotDLens,
    snapshotEpochToScaleToSumTokenName,
    toSPInteger,
    (|*|),
    (|+|),
    (|-|),
    (|/|),
  )
import Indigo.Utils.Helpers (getTokenName, unitValue)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Model
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (foldMap, mconcat, mempty, pure, (<$>), (<>))
import Spec.Collector.Transactions (collectorFeeTx, runInitCollector)
import Spec.Governance.Helpers (findGov)
import Spec.Governance.Transactions (runInitGov)
import Spec.StabilityPool.Helpers
  ( adjustmentHelper,
    findAccount,
    findStabilityPool,
  )
import Spec.StabilityPool.Params
  ( CloseParam (CloseParam, clTokenName),
    CreateParam (CreateParam, cAmount, cTokenName),
    DepositParam (DepositParam, dAmount, dTokenName),
    RecordEpochToScaleToSumParam (RecordEpochToScaleToSumParam, ressTokenName),
    WithdrawParam (WithdrawParam, wAmount, wTokenName),
  )
import Spec.StabilityPool.Script (stabilityPoolScript)
import Utils.Helpers (getFirstRefScript, initScriptRef, minLovelacesPerUtxo)
import Utils.MintPolicies (authPolicy)
import Utils.Mock qualified as Mock
import Prelude (Int, foldMap, mconcat, mempty, pure, (<$>), (<>))

data RecordVariation
  = RecordSucceed
  | StealAccountToken V2.PubKeyHash V2.TokenName

data OpenVariation
  = OpenSucceed
  | ModifyPoolStakingCredential

-- prereqs for adjust & close actions
runInitialize :: Int -> Run ()
runInitialize numCollectorUtxos = do
  runInitCollector numCollectorUtxos Nothing
  runInitGov

runInitRefScript :: Run ()
runInitRefScript = initScriptRef (stabilityPoolScript Mock.spParams)

runInitStabilityPool :: HasCallStack => Ledger.TokenName -> Run ()
runInitStabilityPool token = do
  runInitRefScript
  admin <- getMainUser
  let val =
        unitValue (stabilityPoolToken Mock.spParams)
          <> Ada.lovelaceValueOf minLovelacesPerUtxo
  sp <- spend admin val
  let tx =
        mconcat
          [ userSpend sp,
            payToRef
              (stabilityPoolScript Mock.spParams)
              (InlineDatum spDatum)
              val
          ]
  void $ signTx admin tx >>= sendTx
  where
    spDatum =
      StabilityPoolDatum
        { spIAsset = token,
          spSnapshot = initSPSnapshot,
          epochToScaleToSum = initEpochToScaleToSumMap
        }

runCreate ::
  HasCallStack =>
  Ledger.PubKeyHash ->
  CreateParam ->
  OpenVariation ->
  Run ()
runCreate user CreateParam {cAmount, cTokenName} variation = do
  admin <- getMainUser
  refScriptUtxo <- getFirstRefScript (stabilityPoolScript Mock.spParams)
  let adaSpendVal =
        Ada.lovelaceValueOf minLovelacesPerUtxo
          <> Ada.lovelaceValueOf (accountCreateFeeLovelaces Mock.spParams)
      spendVal =
        adaSpendVal
          <> Value.assetClassValue
            (Value.assetClass (cdpAssetSymbol Mock.cdpParams) cTokenName)
            cAmount
  sp <- sendValue admin adaSpendVal user >> spend user spendVal
  tx <- createTx refScriptUtxo variation user
  void $ signTx user (userSpend sp <> tx) >>= sendTx
  where
    accountTk = accountToken Mock.spParams
    accountTokenName = getTokenName accountTk

    createTx :: V2.TxOutRef -> OpenVariation -> Ledger.PubKeyHash -> Run Tx
    createTx refScriptUtxo var pkh = do
      (oref, o, snapshot, epochToScaleToSum) <- findStabilityPool cTokenName
      let accountSnapshot =
            snapshot
              { snapshotD = toSPInteger cAmount
              }
          newDeposit = snapshotD snapshot |+| toSPInteger cAmount
          poolSnapshot =
            snapshot
              { snapshotD = newDeposit,
                snapshotS =
                  snapshotS snapshot
                    |+| ((toSPInteger (accountCreateFeeLovelaces Mock.spParams) |*| snapshotP snapshot) |/| newDeposit)
              }
          newEpochToScaleToSum =
            AssocMap.insert
              (snapshotEpoch snapshot, snapshotScale snapshot)
              (snapshotS poolSnapshot)
              epochToScaleToSum
          poolOutputDatum =
            InlineDatum $
              StabilityPoolDatum
                cTokenName
                poolSnapshot
                newEpochToScaleToSum
          poolOutputValue =
            V2.txOutValue o
              <> Value.assetClassValue
                (Value.assetClass (cdpAssetSymbol Mock.cdpParams) cTokenName)
                cAmount
              <> Ada.lovelaceValueOf (accountCreateFeeLovelaces Mock.spParams)
          payToSPPool = case var of
            OpenSucceed ->
              payToRef
                (stabilityPoolScript Mock.spParams)
                poolOutputDatum
                poolOutputValue
            ModifyPoolStakingCredential ->
              payToRef
                (appendStakingPubKey pkh (stabilityPoolScript Mock.spParams))
                poolOutputDatum
                poolOutputValue
      pure $
        mconcat
          [ mintValue
              (authPolicy (stabilityPoolToken Mock.spParams) accountTokenName)
              ()
              (unitValue accountTk),
            spendScriptRef
              refScriptUtxo
              (stabilityPoolScript Mock.spParams)
              oref
              (CreateAccount (Ledger.PaymentPubKeyHash user) cAmount)
              (StabilityPoolDatum cTokenName snapshot epochToScaleToSum),
            payToSPPool,
            payToRef
              (stabilityPoolScript Mock.spParams)
              ( InlineDatum $
                  AccountDatum
                    (Ledger.PaymentPubKeyHash user)
                    cTokenName
                    accountSnapshot
              )
              ( unitValue (accountToken Mock.spParams)
                  <> Ada.lovelaceValueOf minLovelacesPerUtxo
              )
          ]

runDeposit :: HasCallStack => Ledger.PubKeyHash -> DepositParam -> Run ()
runDeposit user DepositParam {dTokenName, dAmount} = do
  when (dAmount <= 0) $ logError "When depositing, cannot have positive or zero"
  runAdjust user dTokenName dAmount

runWithdraw :: HasCallStack => Ledger.PubKeyHash -> WithdrawParam -> Run ()
runWithdraw user WithdrawParam {wTokenName, wAmount} = do
  when (wAmount < 0) $ logError "When withdrawing, cannot have negative amount"
  runAdjust user wTokenName (negate wAmount)

runAdjust ::
  HasCallStack => Ledger.PubKeyHash -> Ledger.TokenName -> Integer -> Run ()
runAdjust user asset change = do
  refScriptUtxo <- getFirstRefScript (stabilityPoolScript Mock.spParams)
  (oref, o, snapshot, epochToScaleToSum) <- findStabilityPool asset
  (aoref, ao, asnapshot) <- findAccount (Ledger.PaymentPubKeyHash user) asset
  (govOref, _, govDatum) <- findGov
  admin <- getMainUser
  sendValue
    admin
    (Ada.lovelaceValueOf (accountAdjustmentFeeLovelaces Mock.spParams))
    user
  sp <-
    spend
      user
      ( Ada.lovelaceValueOf (accountAdjustmentFeeLovelaces Mock.spParams)
          <> if change < 0
            then mempty
            else
              Value.assetClassValue
                (Value.assetClass (assetSymbol Mock.spParams) asset)
                change
      )
  (newAccountSnapshot, reward, refInputs) <-
    over
      (_1 . snapshotDLens)
      (|+| toSPInteger change)
      <$> adjustmentHelper asset snapshot epochToScaleToSum asnapshot
  let newDeposit = snapshotD snapshot |+| toSPInteger change
      poolSnapshot =
        snapshot
          { snapshotD = newDeposit,
            snapshotS = snapshotS snapshot |+| ((toSPInteger (accountAdjustmentFeeLovelaces Mock.spParams) |*| snapshotP snapshot) |/| newDeposit)
          }
      newEpochToScaleToSum =
        AssocMap.insert
          (snapshotEpoch snapshot, snapshotScale snapshot)
          (snapshotS poolSnapshot)
          epochToScaleToSum
      stabilityPoolValue =
        V2.txOutValue o
          <> Value.assetClassValue
            (Value.assetClass (assetSymbol Mock.spParams) asset)
            change
          <> Ada.lovelaceValueOf (negate reward)
          <> Ada.lovelaceValueOf (accountAdjustmentFeeLovelaces Mock.spParams)
      protocolFeePercentage =
        GovParams.protocolFeePercentage . GovParams.protocolParams $ govDatum
      fee = protocolFee protocolFeePercentage reward 0
  feeTx <- collectorFeeTx 0 (Ada.lovelaceValueOf fee)
  let tx =
        mconcat
          [ foldMap refInputInline refInputs,
            userSpend sp,
            spendScriptRef
              refScriptUtxo
              (stabilityPoolScript Mock.spParams)
              oref
              AdjustAccount {aaDepositChange = change}
              (StabilityPoolDatum asset snapshot epochToScaleToSum),
            spendScriptRef
              refScriptUtxo
              (stabilityPoolScript Mock.spParams)
              aoref
              SpendAccount
              (AccountDatum (Ledger.PaymentPubKeyHash user) asset asnapshot),
            payToRef
              (stabilityPoolScript Mock.spParams)
              ( InlineDatum $
                  StabilityPoolDatum
                    asset
                    poolSnapshot
                    newEpochToScaleToSum
              )
              stabilityPoolValue,
            payToRef
              (stabilityPoolScript Mock.spParams)
              ( InlineDatum $
                  AccountDatum
                    (Ledger.PaymentPubKeyHash user)
                    asset
                    newAccountSnapshot
              )
              (V2.txOutValue ao),
            refInputInline govOref,
            payToKey
              user
              ( Ada.lovelaceValueOf (reward - fee)
                  <> if change < 0
                    then
                      negate
                        ( Value.assetClassValue
                            (Value.assetClass (assetSymbol Mock.spParams) asset)
                            change
                        )
                    else mempty
              ),
            feeTx
          ]
  void $ signTx user tx >>= sendTx

runClose :: HasCallStack => Ledger.PubKeyHash -> CloseParam -> Run ()
runClose user CloseParam {clTokenName} = do
  refScriptUtxo <- getFirstRefScript (stabilityPoolScript Mock.spParams)
  (oref, o, snapshot, epochToScaleToSum) <- findStabilityPool clTokenName
  (aoref, _, asnapshot) <-
    findAccount (Ledger.PaymentPubKeyHash user) clTokenName
  (govOref, _, govDatum) <- findGov
  (newAccountSnapshot, reward, refInputs) <-
    adjustmentHelper clTokenName snapshot epochToScaleToSum asnapshot
  let fund = view snapshotDLens newAccountSnapshot
      poolSnapshot =
        StabilityPoolSnapshot
          (snapshotP snapshot)
          (snapshotD snapshot |-| fund)
          (snapshotS snapshot)
          (snapshotEpoch snapshot)
          (snapshotScale snapshot)
      accountTk = accountToken Mock.spParams
      accountTokenName = getTokenName accountTk
      protocolFeePercentage =
        GovParams.protocolFeePercentage . GovParams.protocolParams $ govDatum
      fee = protocolFee protocolFeePercentage reward 0
  feeTx <- collectorFeeTx 0 (Ada.lovelaceValueOf fee)
  let tx =
        mconcat
          [ foldMap refInputInline refInputs,
            spendScriptRef
              refScriptUtxo
              (stabilityPoolScript Mock.spParams)
              oref
              Close
              (StabilityPoolDatum clTokenName snapshot epochToScaleToSum),
            spendScriptRef
              refScriptUtxo
              (stabilityPoolScript Mock.spParams)
              aoref
              SpendAccount
              ( AccountDatum
                  (Ledger.PaymentPubKeyHash user)
                  clTokenName
                  asnapshot
              ),
            mintValue
              (authPolicy (stabilityPoolToken Mock.spParams) accountTokenName)
              ()
              (Value.assetClassValue (accountToken Mock.spParams) (-1)),
            payToRef
              (stabilityPoolScript Mock.spParams)
              ( InlineDatum
                  StabilityPoolDatum
                    { spIAsset = clTokenName,
                      spSnapshot = poolSnapshot,
                      epochToScaleToSum = epochToScaleToSum
                    }
              )
              ( V2.txOutValue o
                  <> Ada.lovelaceValueOf (negate reward)
                  <> Value.singleton
                    (assetSymbol Mock.spParams)
                    clTokenName
                    (negate . fromSPInteger $ fund)
              ),
            payToKey
              user
              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                  <> Ada.lovelaceValueOf (reward - fee)
                  <> Value.singleton
                    (assetSymbol Mock.spParams)
                    clTokenName
                    (fromSPInteger fund)
              ),
            refInputInline govOref,
            feeTx
          ]
  void $ signTx user tx >>= sendTx

runRecordEpochToScaleToSum ::
  HasCallStack =>
  Ledger.PubKeyHash ->
  RecordEpochToScaleToSumParam ->
  RecordVariation ->
  Run ()
runRecordEpochToScaleToSum
  user
  RecordEpochToScaleToSumParam {ressTokenName}
  variation = do
    (oref, o, snapshot, epochToScaleToSum) <- findStabilityPool ressTokenName
    admin <- getMainUser
    let spendVal = Ada.lovelaceValueOf minLovelacesPerUtxo
    sp <- sendValue admin spendVal user >> spend user spendVal
    extraTx <- case variation of
      StealAccountToken user' asset -> do
        (aoref, ao, asnapshot) <-
          findAccount (Ledger.PaymentPubKeyHash user') asset
        pure $
          spendScript
            (stabilityPoolScript Mock.spParams)
            aoref
            SpendAccount
            (AccountDatum (Ledger.PaymentPubKeyHash user') asset asnapshot)
            <> payToKey user (V2.txOutValue ao)
      _ -> pure mempty
    let (remainingMapItem, snapshotMapItems) =
          partitionEpochToScaleToSumMap snapshot epochToScaleToSum
        snapshotEssTokenVal =
          unitValue (snapshotEpochToScaleToSumToken Mock.spParams)
        tx =
          mconcat
            [ userSpend sp,
              spendScript
                (stabilityPoolScript Mock.spParams)
                oref
                RecordEpochToScaleToSum
                ( StabilityPoolDatum
                    { spIAsset = ressTokenName,
                      spSnapshot = snapshot,
                      epochToScaleToSum
                    }
                ),
              payToScript
                (stabilityPoolScript Mock.spParams)
                ( InlineDatum
                    StabilityPoolDatum
                      { spIAsset = ressTokenName,
                        spSnapshot = snapshot,
                        epochToScaleToSum = AssocMap.fromList [remainingMapItem]
                      }
                )
                (V2.txOutValue o),
              mintValue
                ( authPolicy
                    (stabilityPoolToken Mock.spParams)
                    snapshotEpochToScaleToSumTokenName
                )
                ()
                snapshotEssTokenVal,
              payToScript
                (stabilityPoolScript Mock.spParams)
                ( InlineDatum
                    SnapshotEpochToScaleToSumDatum
                      { sessSnapshot = AssocMap.fromList snapshotMapItems,
                        sessAsset = ressTokenName
                      }
                )
                (snapshotEssTokenVal <> Ada.lovelaceValueOf minLovelacesPerUtxo)
            ]
            <> extraTx
    void $ signTx user tx >>= sendTx

runSpendAccountWithSP :: HasCallStack => Value.TokenName -> Run ()
runSpendAccountWithSP tokenName = do
  admin <- getMainUser
  refScriptUtxo <- getFirstRefScript (stabilityPoolScript Mock.spParams)
  (oref, o, snapshot, epochToScaleToSum) <- findStabilityPool tokenName
  let tx =
        mconcat
          [ spendScriptRef
              refScriptUtxo
              (stabilityPoolScript Mock.spParams)
              oref
              SpendAccount
              (StabilityPoolDatum tokenName snapshot epochToScaleToSum),
            payToKey
              admin
              ( V2.txOutValue o
              )
          ]
  void $ signTx admin tx >>= sendTx
