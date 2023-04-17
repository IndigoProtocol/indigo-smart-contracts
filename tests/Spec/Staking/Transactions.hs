{-# LANGUAGE NamedFieldPuns #-}

module Spec.Staking.Transactions
  ( OpenPositionVariation
      ( OpenSucceed,
        OpSignedByOtherUser,
        OpAdditionalStakingPositionMint,
        OpIncorrectTotalStake,
        OpIncorrectPositionSnapshot,
        OpDoubleSatisfaction,
        OpWithDistribute
      ),
    runInitStaking,
    runOpenStakingPos,
    runUnstake,
    runStake,
    runDistribute,
    runUnlock,
    runDoubleSatisfactionUnlock,
    DistributeVariation
      ( MintStakingToken,
        DistributeSucceed,
        DistributeRedeemerOtherScript,
        DistributeWithStakingKey,
        DistributeLess,
        DistributeNoCollectorInput
      ),
    UnstakeVariation
      ( UnstakeSucceed,
        UnstakeExtraRewards,
        UnstakeWithDistribute
      ),
    UnlockVariation
      ( UnlockSucceed,
        UnlockWithDistribute
      ),
    StakeVariation
      ( StakeSucceed,
        StakeWithDistribute
      ),
    runInitRefScript,
  )
where

import Control.Monad (void, when)
import GHC.Stack (HasCallStack)
import Indigo.Contracts.Collector.Common (CollectorRedeemer (Collect))
import Indigo.Contracts.Staking.Common
  ( RewardSnapshot (RewardSnapshot, snapshotAda),
    StakingDatum
      ( StakingManager,
        StakingPosition,
        lockedAmount,
        mSnapshot,
        owner,
        pSnapshot,
        totalStake
      ),
    StakingRedeemer
      ( AdjustStakedAmount,
        CreateStakingPosition,
        Distribute,
        Unlock,
        Unstake,
        UpdateTotalStake
      ),
    distributeReward,
    getReward,
  )
import Indigo.Contracts.Staking.Common qualified as StakingParams
import Indigo.Utils.Helpers (getTokenName, unitValue)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Model
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.AssocMap qualified as PtMap
import PlutusTx.Prelude hiding (mconcat, mempty, pure, (<$>), (<>))
import Spec.Collector.Script (collectorScript)
import Spec.Staking.Helpers
  ( findAllStakingPositions,
    findStakingManager,
    findStakingPosition,
  )
import Spec.Staking.Params
  ( OpenStakingPositionParam (OpenStakingPositionParam, oAmount),
    StakeParam (StakeParam, sAmount),
  )
import Spec.Staking.Script (alwaysPassingValidator, stakingScript)
import Utils.Helpers (getFirstRefScript, initScriptRef, minLovelacesPerUtxo)
import Utils.MintPolicies (authPolicy)
import Utils.Mock qualified as Mock
import Prelude (fromIntegral, mconcat, mempty, pure, (<$>), (<>))

data DistributeVariation
  = DistributeSucceed
  | MintStakingToken
  | DistributeRedeemerOtherScript V2.TxOut V2.TxOutRef
  | DistributeWithStakingKey (Maybe V2.PubKeyHash) (Maybe V2.PubKeyHash)
  | DistributeLess Integer
  | DistributeNoCollectorInput

data OpenPositionVariation
  = OpenSucceed
  | OpSignedByOtherUser
  | OpAdditionalStakingPositionMint
  | OpIncorrectTotalStake
  | OpIncorrectPositionSnapshot
  | OpDoubleSatisfaction
  | OpWithDistribute Integer

data UnstakeVariation
  = UnstakeSucceed
  | UnstakeExtraRewards
  | UnstakeWithDistribute Integer

data StakeVariation
  = StakeSucceed
  | StakeWithDistribute Integer

data UnlockVariation
  = UnlockSucceed
  | UnlockWithDistribute Integer

runInitRefScript :: Run ()
runInitRefScript = initScriptRef (stakingScript Mock.stakeParams)

runInitStaking :: HasCallStack => Run ()
runInitStaking = do
  runInitRefScript
  admin <- getMainUser
  let stakingManagerNft = StakingParams.stakingManagerNFT Mock.stakeParams
      spendVal =
        unitValue stakingManagerNft <> Ada.lovelaceValueOf minLovelacesPerUtxo
      tx =
        mconcat
          [ payToRef
              (stakingScript Mock.stakeParams)
              ( InlineDatum
                  StakingManager
                    { totalStake = 0,
                      mSnapshot = (RewardSnapshot {snapshotAda = 0})
                    }
              )
              spendVal
          ]
  sp <- spend admin spendVal
  void $ signTx admin (userSpend sp <> tx) >>= sendTx

runOpenStakingPos ::
  HasCallStack => Ledger.PubKeyHash -> OpenStakingPositionParam -> OpenPositionVariation -> Run ()
runOpenStakingPos user OpenStakingPositionParam {oAmount} variation = do
  refScriptUtxo <- getFirstRefScript (stakingScript Mock.stakeParams)
  aUser <- newUser mempty
  (oref, o, stakingManagerDatum@StakingManager {totalStake, mSnapshot}) <-
    findStakingManager
  let stakingTk = StakingParams.stakingToken Mock.stakeParams
      stakingTkTN = getTokenName stakingTk
      positionSpendVal =
        Value.assetClassValue (StakingParams.indyToken Mock.stakeParams) oAmount
          <> Ada.lovelaceValueOf minLovelacesPerUtxo
      stakingAccount = case variation of
        OpSignedByOtherUser -> aUser
        _ -> user
      mintStakePositionValue = case variation of
        OpAdditionalStakingPositionMint -> Value.assetClassValue stakingTk 2
        _ -> unitValue stakingTk
      smTotalStake = case variation of
        OpIncorrectTotalStake -> totalStake + oAmount - 10
        _ -> totalStake + oAmount
      mSnapshot' = case variation of
        OpIncorrectPositionSnapshot ->
          RewardSnapshot {snapshotAda = snapshotAda mSnapshot + 10}
        _ -> mSnapshot
  extraTestTx <- case variation of
    OpDoubleSatisfaction -> do
      (oref', o', stakingPositionDatum) <-
        findStakingPosition (Ledger.PaymentPubKeyHash user)
      pure $
        mconcat
          [ spendScriptRef
              refScriptUtxo
              (stakingScript Mock.stakeParams)
              oref'
              Unlock
              stakingPositionDatum,
            payToKey user (V2.txOutValue o')
          ]
    OpWithDistribute amt -> do
      rewardUtxos <-
        filter condition <$> utxoAt (collectorScript Mock.collectorParams)
      refScriptUtxoCollector <- getFirstRefScript (collectorScript Mock.collectorParams)
      pure $
        mconcat
          [ mconcat $
              map
                ( \(ocref, _) ->
                    spendScriptRef
                      refScriptUtxoCollector
                      (collectorScript Mock.collectorParams)
                      ocref
                      Collect
                      ()
                )
                rewardUtxos,
            payToKey user (Ada.lovelaceValueOf amt)
          ]
    _ -> pure mempty
  let tx =
        mconcat
          [ spendScriptRef
              refScriptUtxo
              (stakingScript Mock.stakeParams)
              oref
              (CreateStakingPosition (Ledger.PaymentPubKeyHash stakingAccount))
              stakingManagerDatum,
            mintValue
              ( authPolicy
                  (StakingParams.stakingManagerNFT Mock.stakeParams)
                  stakingTkTN
              )
              ()
              mintStakePositionValue,
            payToRef
              (stakingScript Mock.stakeParams)
              ( InlineDatum
                  stakingManagerDatum {totalStake = smTotalStake}
              )
              (V2.txOutValue o),
            payToRef
              (stakingScript Mock.stakeParams)
              ( InlineDatum
                  StakingPosition
                    { owner = Ledger.PaymentPubKeyHash user,
                      lockedAmount = PtMap.empty,
                      pSnapshot = mSnapshot'
                    }
              )
              (mintStakePositionValue <> positionSpendVal),
            extraTestTx
          ]
  admin <- getMainUser
  sp <-
    sendValue admin positionSpendVal user
      >> spend user positionSpendVal
  void $ signTx user (userSpend sp <> tx) >>= sendTx
  where
    condition :: (V2.TxOutRef, V2.TxOut) -> Bool
    condition (_, o) =
      (Ada.getLovelace . Ada.fromValue) (V2.txOutValue o) >= minLovelacesPerUtxo

runUnstake :: HasCallStack => Ledger.PubKeyHash -> UnstakeVariation -> Run ()
runUnstake user variation = do
  refScriptUtxo <- getFirstRefScript (stakingScript Mock.stakeParams)
  (oref, o, stakingManagerDatum@StakingManager {totalStake, mSnapshot}) <-
    findStakingManager
  (oref', o', stakingPositionDatum@StakingPosition {pSnapshot}) <-
    findStakingPosition (Ledger.PaymentPubKeyHash user)
  extraTestTx <- case variation of
    UnstakeWithDistribute amt -> do
      rewardUtxos <-
        filter condition <$> utxoAt (collectorScript Mock.collectorParams)
      refScriptUtxoCollector <- getFirstRefScript (collectorScript Mock.collectorParams)
      pure $
        mconcat
          [ mconcat $
              map
                ( \(ocref, _) ->
                    spendScriptRef
                      refScriptUtxoCollector
                      (collectorScript Mock.collectorParams)
                      ocref
                      Collect
                      ()
                )
                rewardUtxos,
            payToKey user (Ada.lovelaceValueOf amt)
          ]
    _ -> pure mempty

  let stakeValue =
        Value.assetClassValueOf
          (V2.txOutValue o')
          (StakingParams.indyToken Mock.stakeParams)
      reward = getReward pSnapshot mSnapshot stakeValue
      claimedAda = case variation of
        UnstakeExtraRewards -> reward <> Ada.lovelaceValueOf 1
        _ -> reward
      stakingToken = StakingParams.stakingToken Mock.stakeParams
      stakingTokenName = getTokenName stakingToken
      stakingTokenValue = Value.assetClassValue stakingToken (-1)
      tx =
        mconcat
          [ spendScriptRef
              refScriptUtxo
              (stakingScript Mock.stakeParams)
              oref
              UpdateTotalStake
              stakingManagerDatum,
            spendScriptRef
              refScriptUtxo
              (stakingScript Mock.stakeParams)
              oref'
              Unstake
              stakingPositionDatum,
            mintValue
              ( authPolicy
                  (StakingParams.stakingManagerNFT Mock.stakeParams)
                  stakingTokenName
              )
              ()
              stakingTokenValue,
            payToRef
              (stakingScript Mock.stakeParams)
              ( InlineDatum
                  stakingManagerDatum {totalStake = totalStake - stakeValue}
              )
              (V2.txOutValue o <> inv claimedAda),
            payToKey user (V2.txOutValue o' <> stakingTokenValue <> claimedAda),
            extraTestTx
          ]
  void $ signTx user tx >>= sendTx
  where
    condition :: (V2.TxOutRef, V2.TxOut) -> Bool
    condition (_, o) =
      (Ada.getLovelace . Ada.fromValue) (V2.txOutValue o) >= minLovelacesPerUtxo

runStake :: HasCallStack => Ledger.PubKeyHash -> StakeParam -> StakeVariation -> Run ()
runStake user StakeParam {sAmount} variation = do
  refScriptUtxo <- getFirstRefScript (stakingScript Mock.stakeParams)
  (oref, o, stakingManagerDatum@StakingManager {totalStake, mSnapshot}) <-
    findStakingManager
  (oref', o', stakingPositionDatum@StakingPosition {pSnapshot}) <-
    findStakingPosition (Ledger.PaymentPubKeyHash user)
  let stakeValue =
        Value.assetClassValue (StakingParams.indyToken Mock.stakeParams) sAmount
  sp <-
    ( getMainUser >>= \admin ->
        when
          (sAmount > 0)
          (sendValue admin stakeValue user)
      )
      >> spend user stakeValue
  extraTestTx <- case variation of
    StakeWithDistribute amt -> do
      rewardUtxos <-
        filter condition <$> utxoAt (collectorScript Mock.collectorParams)
      refScriptUtxoCollector <- getFirstRefScript (collectorScript Mock.collectorParams)
      pure $
        mconcat
          [ mconcat $
              map
                ( \(ocref, _) ->
                    spendScriptRef
                      refScriptUtxoCollector
                      (collectorScript Mock.collectorParams)
                      ocref
                      Collect
                      ()
                )
                rewardUtxos,
            payToKey user (Ada.lovelaceValueOf amt)
          ]
    _ -> pure mempty
  let reward =
        getReward
          pSnapshot
          mSnapshot
          ( Value.assetClassValueOf
              (V2.txOutValue o')
              (StakingParams.indyToken Mock.stakeParams)
          )
      tx =
        mconcat
          [ userSpend sp,
            spendScriptRef
              refScriptUtxo
              (stakingScript Mock.stakeParams)
              oref
              UpdateTotalStake
              stakingManagerDatum,
            spendScriptRef
              refScriptUtxo
              (stakingScript Mock.stakeParams)
              oref'
              (AdjustStakedAmount sAmount)
              stakingPositionDatum,
            payToRef
              (stakingScript Mock.stakeParams)
              ( InlineDatum
                  stakingManagerDatum {totalStake = totalStake + sAmount}
              )
              (V2.txOutValue o <> inv reward),
            payToRef
              (stakingScript Mock.stakeParams)
              ( InlineDatum
                  stakingPositionDatum
                    { owner = Ledger.PaymentPubKeyHash user,
                      pSnapshot = mSnapshot
                    }
              )
              (stakeValue <> V2.txOutValue o'),
            payToKey user reward,
            extraTestTx
          ]
  void $ signTx user tx >>= sendTx
  where
    condition :: (V2.TxOutRef, V2.TxOut) -> Bool
    condition (_, o) =
      (Ada.getLovelace . Ada.fromValue) (V2.txOutValue o) >= minLovelacesPerUtxo

runDistribute :: HasCallStack => DistributeVariation -> Run ()
runDistribute variation = do
  admin <- getMainUser
  refScriptUtxoStaking <- getFirstRefScript (stakingScript Mock.stakeParams)
  refScriptUtxoCollector <-
    getFirstRefScript (collectorScript Mock.collectorParams)
  let extraTestTx = case variation of
        MintStakingToken ->
          mintValue
            ( authPolicy
                (StakingParams.stakingManagerNFT Mock.stakeParams)
                stakingTokenName
            )
            ()
            stakingTokenValue
            <> payToKey admin stakingTokenValue
        _ -> mempty
      stakingToken = StakingParams.stakingToken Mock.stakeParams
      stakingTokenName = getTokenName stakingToken
      stakingTokenValue = unitValue stakingToken
  rewardUtxosNoStaking <- case variation of
    DistributeNoCollectorInput -> pure []
    _ -> filter condition <$> utxoAt (collectorScript Mock.collectorParams)
  rewardUtxosWithStaking <- case variation of
    DistributeWithStakingKey (Just user) _ ->
      filter condition
        <$> utxoAt
          (appendStakingPubKey user $ collectorScript Mock.collectorParams)
    _ -> pure []
  (oref, o, stakingManagerDatum@StakingManager {totalStake, mSnapshot}) <-
    findStakingManager
  let rewardUtxos = rewardUtxosNoStaking <> rewardUtxosWithStaking
      adaRewardCollected = case variation of
        DistributeLess amt -> amt
        _ ->
          Ada.getLovelace
            (Ada.fromValue $ foldMap (V2.txOutValue . snd) rewardUtxos)
            - fromIntegral (length rewardUtxos) * minLovelacesPerUtxo
      newSnapshot =
        if totalStake > 0
          then distributeReward mSnapshot adaRewardCollected totalStake
          else mSnapshot
      datum = InlineDatum ()
      returnFeePots = case variation of
        DistributeLess amt ->
          let (utxo : utxos, rest) =
                partition
                  (\(_, oc) -> (Ada.getLovelace . Ada.fromValue . V2.txOutValue) oc >= minLovelacesPerUtxo + amt)
                  rewardUtxos
           in mconcat
                [ payToRef
                    (collectorScript Mock.collectorParams)
                    datum
                    (V2.txOutValue (snd utxo) <> negate (Ada.lovelaceValueOf amt)),
                  mconcat
                    ( map
                        ( \(_, oc) ->
                            payToRef
                              (collectorScript Mock.collectorParams)
                              datum
                              (V2.txOutValue oc)
                        )
                        (rest ++ utxos)
                    )
                ]
        _ ->
          mconcat $
            map
              ( \(_, oc) ->
                  let val =
                        Ada.lovelaceValueOf minLovelacesPerUtxo
                          <> (Value.noAdaValue . V2.txOutValue) oc
                   in case variation of
                        DistributeWithStakingKey _ (Just pkh) ->
                          payToRef
                            (appendStakingPubKey pkh $ collectorScript Mock.collectorParams)
                            datum
                            val
                        _ -> payToRef (collectorScript Mock.collectorParams) datum val
              )
              rewardUtxos

      tx =
        mconcat
          [ case variation of
              DistributeRedeemerOtherScript o' oref' ->
                mconcat
                  [ spendScriptUntyped
                      alwaysPassingValidator
                      oref'
                      (V2.Redeemer $ V2.toBuiltinData Distribute)
                      (V2.Datum $ V2.toBuiltinData ()),
                    payToScriptUntyped
                      alwaysPassingValidator
                      datum
                      (V2.txOutValue o' <> Ada.lovelaceValueOf adaRewardCollected)
                  ]
              _ ->
                mconcat
                  [ spendScriptRef
                      refScriptUtxoStaking
                      (stakingScript Mock.stakeParams)
                      oref
                      Distribute
                      stakingManagerDatum,
                    payToRef
                      (stakingScript Mock.stakeParams)
                      (InlineDatum stakingManagerDatum {mSnapshot = newSnapshot})
                      (V2.txOutValue o <> Ada.lovelaceValueOf adaRewardCollected)
                  ],
            mconcat
              [ spendScriptRef
                  refScriptUtxoCollector
                  (collectorScript Mock.collectorParams)
                  coref
                  Collect
                  ()
                | (coref, _) <- rewardUtxos
              ],
            returnFeePots,
            extraTestTx
          ]
  void $ signTx admin tx >>= sendTx
  where
    condition :: (V2.TxOutRef, V2.TxOut) -> Bool
    condition (_, o) =
      (Ada.getLovelace . Ada.fromValue) (V2.txOutValue o) >= minLovelacesPerUtxo

runUnlock :: HasCallStack => Ledger.PubKeyHash -> UnlockVariation -> Run ()
runUnlock user variation = do
  (oref, o, stakingPositionDatum@StakingPosition {lockedAmount}) <-
    findStakingPosition (Ledger.PaymentPubKeyHash user)
  now <- currentTime
  let newLockedAmount =
        PtMap.filter (\(_, endTime) -> endTime > now) lockedAmount
  extraTestTx <- case variation of
    UnlockWithDistribute amt -> do
      rewardUtxos <-
        filter condition <$> utxoAt (collectorScript Mock.collectorParams)
      refScriptUtxoCollector <- getFirstRefScript (collectorScript Mock.collectorParams)
      pure $
        mconcat
          [ mconcat $
              map
                ( \(ocref, _) ->
                    spendScriptRef
                      refScriptUtxoCollector
                      (collectorScript Mock.collectorParams)
                      ocref
                      Collect
                      ()
                )
                rewardUtxos,
            payToKey user (Ada.lovelaceValueOf amt)
          ]
    _ -> pure mempty
  tx <-
    validateIn
      (Ledger.from now)
      ( mconcat
          [ spendScript
              (stakingScript Mock.stakeParams)
              oref
              Unlock
              stakingPositionDatum,
            payToScript
              (stakingScript Mock.stakeParams)
              ( InlineDatum
                  stakingPositionDatum {lockedAmount = newLockedAmount}
              )
              (V2.txOutValue o),
            extraTestTx
          ]
      )
  void $ signTx user tx >>= sendTx
  where
    condition :: (V2.TxOutRef, V2.TxOut) -> Bool
    condition (_, o) =
      (Ada.getLovelace . Ada.fromValue) (V2.txOutValue o) >= minLovelacesPerUtxo

runDoubleSatisfactionUnlock :: HasCallStack => Ledger.PubKeyHash -> Run ()
runDoubleSatisfactionUnlock user = do
  [(oref1, o1, stakingPositionDatum1), (oref2, o2, stakingPositionDatum2)] <-
    findAllStakingPositions (Ledger.PaymentPubKeyHash user)
  now <- currentTime
  tx <-
    validateIn
      (Ledger.from now)
      ( mconcat
          [ spendScript
              (stakingScript Mock.stakeParams)
              oref1
              Unlock
              stakingPositionDatum1,
            spendScript
              (stakingScript Mock.stakeParams)
              oref2
              Unlock
              stakingPositionDatum2,
            payToScript
              (stakingScript Mock.stakeParams)
              (InlineDatum stakingPositionDatum1)
              (V2.txOutValue o1),
            payToKey user (V2.txOutValue o2)
          ]
      )
  void $ signTx user tx >>= sendTx
