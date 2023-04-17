{-# LANGUAGE NamedFieldPuns #-}

module Spec.CDP.Transactions
  ( OpenPositionVariation
      ( Succeed,
        SignedByOtherUser,
        IncorrectIAssetNFTUsed,
        IncorrectIAssetTokenUsed,
        IncorrectOracleNFTUsed,
        ExtraMint,
        ExtraMintAndReference,
        ExtraMintWithDatum,
        WithStakingCredential
      ),
    AdjustVariation
      ( AdjustSucceed,
        AdjustWithStakingCredential,
        AdjustSignedByOtherUser,
        IncorrectIAssetNFTUsedToAdjust,
        SpendStabilityPoolOutput
      ),
    CloseVariation
      ( CloseSucceed,
        CloseWithStakingCredential,
        CloseSignedByOtherUser,
        BurnMoreAsset,
        BurnLessAsset,
        NoProtocolFee,
        IncorrectProtocolFee
      ),
    FreezeVariation
      ( FreezeAttachStakingCredential,
        FreezeConsumeSP,
        FreezeExtraMint,
        FreezeSucceed,
        FreezeWithStakingCredential
      ),
    LiquidationVariation
      ( LiquidationFakePartial,
        LiquidationSucceed,
        LiquidationStealSPAccount,
        LiquidationMoreBurns,
        LiquidationSPEmpty
      ),
    IAssetType (Delisted, Active),
    runLiquidateExploit,
    runBurn,
    runClose,
    runDeposit,
    runInitialize,
    runMint,
    runOpenCDPPosition,
    createCloseTx,
    runFreeze,
    runLiquidate,
    runWithdraw,
    runMergeCDPs,
    runInitRefScript,
    runInitRefScriptCreator,
  )
where

import Control.Lens (view, _2)
import Control.Monad (void)
import GHC.Stack (HasCallStack)
import Indigo.Contracts.CDP.Common
  ( CDPCreatorRedeemer (CreateCDP),
    CDPDatum (CDP, IAssetDatum),
    CDPRedeemer
      ( AdjustCDP,
        CloseCDP,
        FreezeCDP,
        Liquidate,
        MergeAuxiliary,
        MergeCDPs
      ),
    IAsset (IAsset),
    cdpAssetSymbol,
    cdpIAsset,
    cdpMintedAmount,
    cdpOwner,
    protocolFee,
  )
import Indigo.Contracts.CDP.Common qualified as CDPParams
import Indigo.Contracts.Governance.Gov.Common qualified as GovParams
import Indigo.Contracts.Oracle.Common (OracleAssetNFT (MkOracleAssetNFT))
import Indigo.Contracts.StabilityPool.Common
  ( StabilityDatum (AccountDatum, StabilityPoolDatum),
    StabilityPoolRedeemer (LiquidateCDP, SpendAccount),
    liquidateHelper,
  )
import Indigo.Data.Decimal
import Indigo.Utils.Helpers
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Plutus.Model
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude hiding (mconcat, mempty, pure, (<$>), (<*>), (<>))
import Spec.CDP.Helpers
  ( cdpCreatorValue,
    findCDP,
    findCDPCreator,
    findFrozenCDP,
    findIAsset,
    findIAssetCustomNft,
    iAssetAuthValue,
    openPositionSpendVal,
  )
import Spec.CDP.Params
  ( BurnParam (BurnParam, bAmount, bAsset, bCdpOwnerPkh),
    CloseParam (CloseParam, cAsset, cCdpOwnerPkh),
    DepositParam (DepositParam, dAsset, dCdpOwnerPkh),
    FreezeParam (FreezeParam, fAsset, fCdpOwnerPkh),
    LiquidateParam (LiquidateParam, lAsset),
    MergeCDPsParam (MergeCDPsParam, cdpsToMerge, mpAsset),
    MintParam (MintParam, mAmount, mAsset, mCdpOwnerPkh),
    OpenPositionParam
      ( OpenPositionParam,
        opAsset,
        opCollateralAmount,
        opMintedAmount
      ),
    WithdrawParam (WithdrawParam, wAmount, wAsset, wCdpOwnerPkh),
    dAmount,
  )
import Spec.CDP.Script (cdpCreatorScript, cdpScript, iAssetPolicy)
import Spec.Collector.Transactions (collectorFeeTx, runInitCollector)
import Spec.Governance.Helpers (findGov)
import Spec.Governance.Transactions (runInitGov)
import Spec.Oracle.Helpers (oAssetNFTAssetClass)
import Spec.Oracle.Transactions (startOracle, useOracleTx, useOracleTxCustomNft)
import Spec.StabilityPool.Helpers qualified as SPHelpers
import Spec.StabilityPool.Script (stabilityPoolScript)
import Spec.StabilityPool.Transactions qualified as SP
import Spec.Staking.Transactions (runInitStaking)
import Utils.Helpers
  ( StakingStrategy (IgnoreStaking, UseStakingKey),
    findAllUtxosAt,
    getFirstRefScript,
    initScriptRef,
    minLovelacesPerUtxo,
  )
import Utils.MintPolicies (authPolicy)
import Utils.Mock qualified as Mock
import Prelude (Int, div, mconcat, mempty, pure, (<$>), (<*>), (<>))

data OpenPositionVariation
  = Succeed
  | WithStakingCredential
  | SignedByOtherUser
  | IncorrectIAssetNFTUsed Value.AssetClass
  | IncorrectIAssetTokenUsed Value.TokenName
  | IncorrectOracleNFTUsed Value.AssetClass
  | ExtraMint V2.Value
  | ExtraMintAndReference Value.TokenName Integer
  | ExtraMintWithDatum Integer

data AdjustVariation
  = AdjustSucceed
  | AdjustWithStakingCredential
  | AdjustSignedByOtherUser V2.PubKeyHash
  | IncorrectIAssetNFTUsedToAdjust Value.TokenName
  | SpendStabilityPoolOutput V2.TokenName

data CloseVariation
  = CloseSucceed
  | CloseWithStakingCredential
  | CloseSignedByOtherUser V2.PubKeyHash
  | BurnMoreAsset
  | BurnLessAsset
  | NoProtocolFee
  | IncorrectProtocolFee OnChainDecimal

data FreezeVariation
  = FreezeSucceed
  | FreezeWithStakingCredential
  | FreezeAttachStakingCredential
  | FreezeConsumeSP
  | FreezeExtraMint V2.Value

data LiquidationVariation
  = LiquidationSucceed
  | LiquidationFakePartial
  | LiquidationStealSPAccount Ledger.PubKeyHash
  | LiquidationMoreBurns (Ledger.TokenName, Integer)
  | LiquidationSPEmpty

data IAssetType = Delisted | Active

runInitRefScriptCreator :: Run ()
runInitRefScriptCreator = initScriptRef (cdpCreatorScript Mock.cdpCreatorParams)

runInitRefScript :: Run ()
runInitRefScript = initScriptRef (cdpScript Mock.cdpParams)

runInitialize ::
  HasCallStack =>
  [(Ledger.TokenName, OnChainDecimal, IAssetType)] ->
  Int ->
  Run ()
runInitialize oracles collectorUtxosCount = do
  runInitRefScriptCreator
  runInitRefScript
  runInitCollector collectorUtxosCount Nothing
  runInitCDPCreator
  runInitGov
  runInitStaking
  mapM_ initForToken oracles
  where
    initForToken ::
      (Ledger.TokenName, OnChainDecimal, IAssetType) ->
      Run ()
    initForToken param@(token, price, _) = do
      void $ startOracle token price
      runInitIAsset param
      SP.runInitStabilityPool token

    runInitCDPCreator :: Run ()
    runInitCDPCreator = do
      admin <- getMainUser
      sp <- spend admin cdpCreatorValue
      void $
        signTx
          admin
          ( mconcat
              [ userSpend sp,
                payToRef
                  (cdpCreatorScript Mock.cdpCreatorParams)
                  (InlineDatum ())
                  cdpCreatorValue
              ]
          )
          >>= sendTx

    runInitIAsset :: (Ledger.TokenName, OnChainDecimal, IAssetType) -> Run ()
    runInitIAsset (token, val, iAssetType) = do
      admin <- getMainUser
      sp <- spend admin iAssetAuthValue
      void $
        signTx
          admin
          ( mconcat
              [ userSpend sp,
                payToRef
                  (cdpScript Mock.cdpParams)
                  ( InlineDatum $
                      IAssetDatum
                        ( IAsset
                            token
                            Mock.iaRatio
                            ( case iAssetType of
                                Delisted -> Left val
                                Active ->
                                  Right
                                    ( MkOracleAssetNFT
                                        (oAssetNFTAssetClass token)
                                    )
                            )
                        )
                  )
                  iAssetAuthValue
              ]
          )
          >>= sendTx

runOpenCDPPosition ::
  HasCallStack =>
  Ledger.PubKeyHash ->
  OpenPositionParam ->
  OpenPositionVariation ->
  Run ()
runOpenCDPPosition
  user
  opParams@OpenPositionParam {opAsset, opCollateralAmount, opMintedAmount}
  variation = do
    let iAssetValue = case variation of
          ExtraMint extraToMint ->
            Value.singleton
              (CDPParams.cdpAssetSymbol Mock.cdpParams)
              opAsset
              opMintedAmount
              <> extraToMint
          ExtraMintWithDatum extraToMint ->
            Value.singleton
              (CDPParams.cdpAssetSymbol Mock.cdpParams)
              opAsset
              (opMintedAmount + extraToMint)
          _ ->
            Value.singleton
              (CDPParams.cdpAssetSymbol Mock.cdpParams)
              opAsset
              opMintedAmount
    opTx <- openPositionTx opParams user variation
    void $
      checkBalanceBy
        ( \success ->
            if success
              then
                owns
                  user
                  ( Ada.lovelaceValueOf (negate opCollateralAmount)
                      <> iAssetValue
                  )
              else mempty
        )
        ( do
            void $ sendTx opTx
            noErrors
        )

openPositionTx ::
  OpenPositionParam -> Ledger.PubKeyHash -> OpenPositionVariation -> Run Tx
openPositionTx
  opParams@OpenPositionParam {opAsset, opCollateralAmount, opMintedAmount}
  user
  variation = do
    refScriptUtxoCreator <-
      getFirstRefScript (cdpCreatorScript Mock.cdpCreatorParams)
    let cdpToken = CDPParams.cdpAuthToken Mock.cdpParams
        mintIAssetValue = mintValue (iAssetPolicy cdpToken) ()
        mintIAssetQuantity =
          mintIAssetValue
            . Value.singleton (CDPParams.cdpAssetSymbol Mock.cdpParams) opAsset

    -- Setup of parameters - the happy path goes through the default cases
    let iAssetValue =
          Value.singleton
            (CDPParams.cdpAssetSymbol Mock.cdpParams)
            opAsset
            opMintedAmount
            <> case variation of
              ExtraMint extraToMint -> extraToMint
              ExtraMintAndReference extraIAsset extraAmount ->
                Value.singleton
                  (cdpAssetSymbol Mock.cdpParams)
                  extraIAsset
                  extraAmount
              ExtraMintWithDatum extraToMint
                | extraToMint >= 0 ->
                    Value.singleton
                      (CDPParams.cdpAssetSymbol Mock.cdpParams)
                      opAsset
                      extraToMint
              _ -> mempty
        iAssetMintValue =
          case variation of
            ExtraMint extraToMint ->
              mintIAssetQuantity opMintedAmount <> mintIAssetValue extraToMint
            ExtraMintAndReference extraIAsset extraAmount ->
              mintIAssetQuantity opMintedAmount
                <> mintIAssetValue
                  ( Value.singleton
                      (cdpAssetSymbol Mock.cdpParams)
                      extraIAsset
                      extraAmount
                  )
            ExtraMintWithDatum extraToMint ->
              if opMintedAmount + extraToMint == 0
                then mempty
                else mintIAssetQuantity (opMintedAmount + extraToMint)
            _ -> mintIAssetQuantity opMintedAmount
        storedIAssetAmount = case variation of
          ExtraMintWithDatum extraToMint -> opMintedAmount + extraToMint
          _ -> opMintedAmount
        spendVal = case variation of
          ExtraMintWithDatum extraToMint
            | extraToMint < 0 ->
                openPositionSpendVal opParams
                  <> Value.singleton
                    (CDPParams.cdpAssetSymbol Mock.cdpParams)
                    opAsset
                    (negate extraToMint)
          _ -> openPositionSpendVal opParams
    signatory <- case variation of
      SignedByOtherUser -> newUser mempty
      _ -> pure user
    oracleTx <- case variation of
      IncorrectOracleNFTUsed oracleAssetNft ->
        useOracleTxCustomNft oracleAssetNft
      ExtraMintAndReference extraIAsset _ ->
        (<>) <$> useOracleTx extraIAsset <*> useOracleTx opAsset
      _ -> useOracleTx opAsset
    referenceAssetTx <- case variation of
      IncorrectIAssetNFTUsed iAssetNft ->
        referenceIAssetCustomNft opAsset iAssetNft
      IncorrectIAssetTokenUsed iAssetName -> referenceIAsset iAssetName
      ExtraMintAndReference extraIAsset _ ->
        (<>) <$> referenceIAsset extraIAsset <*> referenceIAsset opAsset
      _ -> referenceIAsset opAsset

    -- The actual execution
    (oref, _) <- findCDPCreator
    let cdpTokenName = getTokenName cdpToken
        authValue = unitValue (CDPParams.cdpAuthToken Mock.cdpParams)
        payToCDPScript = case variation of
          WithStakingCredential ->
            payToRef
              (appendStakingPubKey user (cdpScript Mock.cdpParams))
              ( InlineDatum $
                  CDP
                    (Just $ Ledger.PaymentPubKeyHash user)
                    opAsset
                    storedIAssetAmount
              )
              (authValue <> Ada.lovelaceValueOf opCollateralAmount)
          _ ->
            payToRef
              (cdpScript Mock.cdpParams)
              ( InlineDatum $
                  CDP
                    (Just $ Ledger.PaymentPubKeyHash user)
                    opAsset
                    storedIAssetAmount
              )
              (authValue <> Ada.lovelaceValueOf opCollateralAmount)
    sp <-
      getMainUser
        >>= (\admin -> sendValue admin spendVal user >> spend user spendVal)
    signTx
      signatory
      ( mconcat
          [ userSpend sp,
            spendScriptRef
              refScriptUtxoCreator
              (cdpCreatorScript Mock.cdpCreatorParams)
              oref
              ( CreateCDP
                  (Ledger.PaymentPubKeyHash user)
                  storedIAssetAmount
                  opCollateralAmount
              )
              (),
            referenceAssetTx,
            mintValue
              ( authPolicy
                  (CDPParams.cdpCreatorNft Mock.cdpCreatorParams)
                  cdpTokenName
              )
              ()
              authValue,
            iAssetMintValue,
            payToRef
              (cdpCreatorScript Mock.cdpCreatorParams)
              (InlineDatum ())
              cdpCreatorValue,
            payToCDPScript,
            payToKey
              user
              (iAssetValue <> Ada.lovelaceValueOf minLovelacesPerUtxo),
            oracleTx
          ]
      )

referenceIAsset :: HasCallStack => Value.TokenName -> Run Tx
referenceIAsset name =
  (\(iaOref, _, _) -> refInputInline iaOref) <$> findIAsset name

referenceIAssetCustomNft ::
  HasCallStack => Value.TokenName -> Value.AssetClass -> Run Tx
referenceIAssetCustomNft name iAssetAuthNft =
  ( \(iaOref, _, _) ->
      refInputInline iaOref
  )
    <$> findIAssetCustomNft name iAssetAuthNft

runDeposit :: HasCallStack => DepositParam -> AdjustVariation -> Run ()
runDeposit
  DepositParam {dCdpOwnerPkh = Ledger.PaymentPubKeyHash user, dAsset, dAmount}
  variation = do
    admin <- getMainUser
    let depositValue = Ada.lovelaceValueOf dAmount
        spendVal =
          case variation of
            SpendStabilityPoolOutput _ ->
              depositValue <> Ada.lovelaceValueOf minLovelacesPerUtxo
            _ -> depositValue
    sp <- sendValue admin spendVal user >> spend user spendVal
    dTx <-
      depositTx (Ledger.PaymentPubKeyHash user) dAsset depositValue variation
    void (sendTx $ userSpend sp <> dTx)

depositTx ::
  Ledger.PaymentPubKeyHash ->
  Ledger.TokenName ->
  Ledger.Value ->
  AdjustVariation ->
  Run Tx
depositTx pkh tn depositValue variation = do
  refScriptUtxo <- getFirstRefScript (cdpScript Mock.cdpParams)
  (cdpOref, cdpO, cdpDatum@CDP {cdpIAsset}) <-
    findCDP
      Mock.cdpParams
      pkh
      ( case variation of
          AdjustWithStakingCredential -> UseStakingKey (Ledger.unPaymentPubKeyHash pkh)
          _ -> IgnoreStaking
      )
      tn
  let cdpTokenName = case variation of
        IncorrectIAssetNFTUsedToAdjust iAssetName -> iAssetName
        _ -> cdpIAsset
      signatory = case variation of
        AdjustSignedByOtherUser user -> user
        _ -> Ledger.unPaymentPubKeyHash pkh
      collateralValue = depositValue <> V2.txOutValue cdpO
      payToCDPScript = case variation of
        AdjustWithStakingCredential ->
          payToRef
            (appendStakingPubKey (Ledger.unPaymentPubKeyHash pkh) (cdpScript Mock.cdpParams))
            (InlineDatum cdpDatum)
            collateralValue
        _ ->
          payToRef
            (cdpScript Mock.cdpParams)
            (InlineDatum cdpDatum)
            collateralValue
  disallowedTx <- case variation of
    SpendStabilityPoolOutput iAssetName -> do
      (oref, o, snapshot, epochToScaleToSum) <-
        SPHelpers.findStabilityPool iAssetName
      pure $
        spendScript
          (stabilityPoolScript Mock.spParams)
          oref
          LiquidateCDP
          (StabilityPoolDatum iAssetName snapshot epochToScaleToSum)
          <> payToScript
            (stabilityPoolScript Mock.spParams)
            ( InlineDatum $
                StabilityPoolDatum iAssetName snapshot epochToScaleToSum
            )
            (Ada.lovelaceValueOf minLovelacesPerUtxo)
          <> payToKey signatory (V2.txOutValue o)
    _ -> pure mempty
  (govOref, _, _) <- findGov
  (iaoref, _, _) <- findIAsset cdpTokenName
  signTx signatory $
    mconcat
      [ spendScriptRef
          refScriptUtxo
          (cdpScript Mock.cdpParams)
          cdpOref
          AdjustCDP
          cdpDatum,
        refInputInline iaoref,
        refInputInline govOref,
        payToCDPScript,
        disallowedTx
      ]

runWithdraw :: HasCallStack => WithdrawParam -> Run ()
runWithdraw
  WithdrawParam
    { wCdpOwnerPkh = Ledger.PaymentPubKeyHash user,
      wAsset,
      wAmount
    } = do
    (_, _, govDatum) <- findGov
    let protocolFeePercentage = getProtocolFeePercentage govDatum
        withdrawAmount = Ada.lovelaceValueOf wAmount
        collectorFee =
          Ada.lovelaceValueOf $ protocolFee protocolFeePercentage wAmount 0
    wTx <-
      withdrawTx
        user
        (Ledger.PaymentPubKeyHash user)
        wAsset
        withdrawAmount
        collectorFee
    void $ signTx user wTx >>= sendTx

withdrawTx ::
  Ledger.PubKeyHash ->
  Ledger.PaymentPubKeyHash ->
  Ledger.TokenName ->
  Ledger.Value ->
  Ledger.Value ->
  Run Tx
withdrawTx user pkh tn withdrawValue collectorFee = do
  refScriptUtxo <- getFirstRefScript (cdpScript Mock.cdpParams)
  (cdpOref, cdpO, cdpDatum@CDP {cdpIAsset}) <- findCDP Mock.cdpParams pkh IgnoreStaking tn
  (govOref, _, _) <- findGov
  (iaoref, _, _) <- findIAsset cdpIAsset
  oracleTx <- useOracleTx cdpIAsset
  feeTx <- collectorFeeTx 0 collectorFee
  let collateralValue = inv withdrawValue <> V2.txOutValue cdpO
  pure $
    mconcat
      [ spendScriptRef
          refScriptUtxo
          (cdpScript Mock.cdpParams)
          cdpOref
          AdjustCDP
          cdpDatum,
        refInputInline iaoref,
        refInputInline govOref,
        payToRef
          (cdpScript Mock.cdpParams)
          (InlineDatum cdpDatum)
          collateralValue,
        payToKey
          user
          ( Ada.toValue
              (Ada.fromValue withdrawValue - Ada.fromValue collectorFee)
          ),
        oracleTx,
        feeTx
      ]

runMint :: HasCallStack => MintParam -> Run ()
runMint
  MintParam
    { mCdpOwnerPkh = Ledger.PaymentPubKeyHash user,
      mAsset,
      mAmount
    } = do
    admin <- getMainUser
    let spendVal = Ada.lovelaceValueOf minLovelacesPerUtxo
    sp <- sendValue admin spendVal user >> spend user spendVal
    tx <- mintTx user (Ledger.PaymentPubKeyHash user) mAsset mAmount
    void $ signTx user (userSpend sp <> tx) >>= sendTx

mintTx ::
  Ledger.PubKeyHash ->
  Ledger.PaymentPubKeyHash ->
  Ledger.TokenName ->
  Integer ->
  Run Tx
mintTx user pkh tn amount = do
  refScriptUtxo <- getFirstRefScript (cdpScript Mock.cdpParams)
  (cdpOref, cdpO, cdpDatum@CDP {cdpIAsset, cdpMintedAmount}) <-
    findCDP Mock.cdpParams pkh IgnoreStaking tn
  (govOref, _, _) <- findGov
  (iaoref, _, _) <- findIAsset cdpIAsset
  oracleTx <- useOracleTx cdpIAsset
  let newMintedAmount = cdpMintedAmount + amount
      collateralValue = V2.txOutValue cdpO
      iAssetValue =
        Value.singleton
          (CDPParams.cdpAssetSymbol Mock.cdpParams)
          cdpIAsset
          amount
  pure $
    mconcat
      [ spendScriptRef
          refScriptUtxo
          (cdpScript Mock.cdpParams)
          cdpOref
          AdjustCDP
          cdpDatum,
        refInputInline iaoref,
        refInputInline govOref,
        payToRef
          (cdpScript Mock.cdpParams)
          (InlineDatum cdpDatum {cdpMintedAmount = newMintedAmount})
          collateralValue,
        mintValue
          (iAssetPolicy (CDPParams.cdpAuthToken Mock.cdpParams))
          ()
          iAssetValue,
        payToKey user (iAssetValue <> Ada.lovelaceValueOf minLovelacesPerUtxo),
        oracleTx
      ]

runBurn :: HasCallStack => BurnParam -> Run ()
runBurn
  BurnParam {bCdpOwnerPkh = Ledger.PaymentPubKeyHash user, bAsset, bAmount} = do
    admin <- getMainUser
    let iAssetBurnValue =
          Value.singleton (cdpAssetSymbol Mock.cdpParams) bAsset bAmount
        spendVal = Ada.lovelaceValueOf minLovelacesPerUtxo
    sp <-
      sendValue admin spendVal user >> spend user (spendVal <> iAssetBurnValue)
    tx <- burnTx (Ledger.PaymentPubKeyHash user) bAsset bAmount
    void $
      signTx
        user
        ( userSpend sp
            <> payToKey user (Ada.lovelaceValueOf minLovelacesPerUtxo)
            <> tx
        )
        >>= sendTx

burnTx :: Ledger.PaymentPubKeyHash -> Ledger.TokenName -> Integer -> Run Tx
burnTx pkh tn amount = do
  refScriptUtxo <- getFirstRefScript (cdpScript Mock.cdpParams)
  (cdpOref, cdpO, cdpDatum@CDP {cdpIAsset, cdpMintedAmount}) <-
    findCDP Mock.cdpParams pkh IgnoreStaking tn
  (govOref, _, _) <- findGov
  (iaoref, _, _) <- findIAsset cdpIAsset
  let newMintedAmount = cdpMintedAmount - amount
      collateralValue = V2.txOutValue cdpO
      iAssetBurnValue =
        Value.singleton (cdpAssetSymbol Mock.cdpParams) cdpIAsset (-amount)
  pure $
    mconcat
      [ spendScriptRef
          refScriptUtxo
          (cdpScript Mock.cdpParams)
          cdpOref
          AdjustCDP
          cdpDatum,
        refInputInline iaoref,
        refInputInline govOref,
        payToRef
          (cdpScript Mock.cdpParams)
          (InlineDatum cdpDatum {cdpMintedAmount = newMintedAmount})
          collateralValue,
        mintValue
          (iAssetPolicy (CDPParams.cdpAuthToken Mock.cdpParams))
          ()
          iAssetBurnValue
      ]

runClose :: HasCallStack => Int -> CloseParam -> CloseVariation -> Run ()
runClose
  collectorUtxoIdxToPick
  param@CloseParam {cCdpOwnerPkh = Ledger.PaymentPubKeyHash user}
  v = do
    tx <- createCloseTx collectorUtxoIdxToPick param v
    void $ signTx signatory tx >>= sendTx
    where
      signatory = case v of
        CloseSignedByOtherUser u -> u
        _ -> user

createCloseTx :: Int -> CloseParam -> CloseVariation -> Run Tx
createCloseTx
  collectorUtxoIdxToPick
  CloseParam {cCdpOwnerPkh = Ledger.PaymentPubKeyHash user, cAsset}
  v = do
    refScriptUtxo <- getFirstRefScript (cdpScript Mock.cdpParams)
    admin <- getMainUser
    (cdpOref, cdpO, cdpDatum@CDP {cdpIAsset, cdpMintedAmount}) <-
      case v of
        CloseWithStakingCredential -> findCDP Mock.cdpParams (Ledger.PaymentPubKeyHash user) (UseStakingKey user) cAsset
        _ -> findCDP Mock.cdpParams (Ledger.PaymentPubKeyHash user) IgnoreStaking cAsset
    (govOref, _, govDatum) <- findGov
    (iaoref, _, _) <- findIAsset cAsset
    let protocolFeePercentage = case v of
          IncorrectProtocolFee f -> f
          _ ->
            GovParams.protocolFeePercentage
              . GovParams.protocolParams
              $ govDatum
        fee = case v of
          NoProtocolFee -> Ada.lovelaceValueOf 0
          _ ->
            Ada.lovelaceValueOf $
              protocolFee
                protocolFeePercentage
                (Ada.getLovelace (Ada.fromValue $ V2.txOutValue cdpO))
                0
        iAssetBurnValue = case v of
          BurnMoreAsset ->
            Value.singleton
              (CDPParams.cdpAssetSymbol Mock.cdpParams)
              cdpIAsset
              (-cdpMintedAmount - 1)
          BurnLessAsset ->
            Value.singleton
              (CDPParams.cdpAssetSymbol Mock.cdpParams)
              cdpIAsset
              (-cdpMintedAmount + 1)
          _ ->
            Value.singleton
              (CDPParams.cdpAssetSymbol Mock.cdpParams)
              cdpIAsset
              (-cdpMintedAmount)
        cdpToken = CDPParams.cdpAuthToken Mock.cdpParams
        authValue = Value.assetClassValue cdpToken (-1)
        cdpTokenName = getTokenName cdpToken
    sp <- sendValue admin fee user >> spend user (fee <> inv iAssetBurnValue)
    feeTx <- case v of
      NoProtocolFee -> pure mempty
      _ -> collectorFeeTx collectorUtxoIdxToPick fee
    pure $
      mconcat
        [ userSpend sp,
          spendScriptRef
            refScriptUtxo
            (cdpScript Mock.cdpParams)
            cdpOref
            CloseCDP
            cdpDatum,
          refInputInline govOref,
          refInputInline iaoref,
          mintValue
            ( authPolicy
                (CDPParams.cdpCreatorNft Mock.cdpCreatorParams)
                cdpTokenName
            )
            ()
            authValue,
          mintValue (iAssetPolicy cdpToken) () iAssetBurnValue,
          payToKey user (Ada.toValue $ Ada.fromValue (V2.txOutValue cdpO)),
          feeTx
        ]

runFreeze :: HasCallStack => FreezeParam -> FreezeVariation -> Run ()
runFreeze FreezeParam {fCdpOwnerPkh = user, fAsset} variation = do
  refScriptUtxo <- getFirstRefScript (cdpScript Mock.cdpParams)
  (cdpOref, cdpO, cdpDatum@CDP {cdpIAsset}) <-
    case variation of
      FreezeWithStakingCredential ->
        findCDP
          Mock.cdpParams
          (Ledger.PaymentPubKeyHash user)
          (UseStakingKey user)
          fAsset
      FreezeAttachStakingCredential ->
        findCDP
          Mock.cdpParams
          (Ledger.PaymentPubKeyHash user)
          (UseStakingKey user)
          fAsset
      _ ->
        findCDP
          Mock.cdpParams
          (Ledger.PaymentPubKeyHash user)
          IgnoreStaking
          fAsset
  extraTestTx <- case variation of
    FreezeConsumeSP -> do
      (orefp, op, snapshot, epochToScaleToSum) <-
        SPHelpers.findStabilityPool cdpIAsset
      let takeAllValue =
            V2.txOutValue op <> Ada.lovelaceValueOf (-minLovelacesPerUtxo)
          (newSnapshot, newEpochToScaleToSum) =
            liquidateHelper
              snapshot
              epochToScaleToSum
              0
              (minLovelacesPerUtxo - lovelacesAmount (V2.txOutValue op))
      pure $
        mconcat
          [ spendScript
              (stabilityPoolScript Mock.spParams)
              orefp
              LiquidateCDP
              (StabilityPoolDatum cdpIAsset snapshot epochToScaleToSum),
            payToScript
              (stabilityPoolScript Mock.spParams)
              ( InlineDatum $
                  StabilityPoolDatum cdpIAsset newSnapshot newEpochToScaleToSum
              )
              (Ada.lovelaceValueOf minLovelacesPerUtxo),
            payToKey user takeAllValue
          ]
    FreezeExtraMint iAssetAmt ->
      pure $
        mintValue
          (iAssetPolicy $ CDPParams.cdpAuthToken Mock.cdpParams)
          ()
          iAssetAmt
          <> payToKey user iAssetAmt
    _ -> pure mempty
  (iaoref, _, _) <- findIAsset cdpIAsset
  oracleTx <- useOracleTx cdpIAsset
  let cdpOutput =
        case variation of
          FreezeAttachStakingCredential ->
            payToRef
              (appendStakingPubKey user (cdpScript Mock.cdpParams))
              (InlineDatum $ cdpDatum {cdpOwner = Nothing})
              (V2.txOutValue cdpO)
          _ ->
            payToRef
              (cdpScript Mock.cdpParams)
              (InlineDatum $ cdpDatum {cdpOwner = Nothing})
              (V2.txOutValue cdpO)
      tx =
        mconcat
          [ spendScriptRef
              refScriptUtxo
              (cdpScript Mock.cdpParams)
              cdpOref
              FreezeCDP
              cdpDatum,
            refInputInline iaoref,
            oracleTx,
            cdpOutput,
            extraTestTx
          ]
  void $ signTx user tx >>= sendTx

runLiquidateExploit ::
  HasCallStack =>
  Ledger.PubKeyHash ->
  LiquidateParam ->
  Run ()
runLiquidateExploit user LiquidateParam {lAsset} = do
  refScriptUtxo <- getFirstRefScript (cdpScript Mock.cdpParams)
  refScriptUtxoSP <- getFirstRefScript (stabilityPoolScript Mock.spParams)
  (cdpOref, cdpO, cdpDatum@CDP {cdpIAsset}) <-
    findFrozenCDP Mock.cdpParams lAsset
  (orefp, op, snapshot, epochToScaleToSum) <-
    SPHelpers.findStabilityPool cdpIAsset

  let createAssetVal :: Integer -> Ledger.Value
      createAssetVal =
        Value.singleton (CDPParams.cdpAssetSymbol Mock.cdpParams) cdpIAsset

      assetAmount =
        Value.assetClassValueOf (V2.txOutValue op) (Value.assetClass (CDPParams.cdpAssetSymbol Mock.cdpParams) cdpIAsset)

      -- Burn all the IAsset in the liquidity pool regardless of cdpMintedAmount.
      burntAmount :: Integer
      burntAmount = assetAmount

      cdpToken = CDPParams.cdpAuthToken Mock.cdpParams
      cdpTokenName = getTokenName cdpToken
      burnValCDPToken =
        inv $ valueOfAssetCls (V2.txOutValue cdpO) cdpToken
      burnValAsset = createAssetVal (-burntAmount)
      collateralAbsorbed =
        Ada.getLovelace . Ada.fromValue . V2.txOutValue $ cdpO
      (newSnapshot, newEpochToScaleToSum) =
        liquidateHelper snapshot epochToScaleToSum burntAmount collateralAbsorbed

      -- New stability pool output value is Input Stability Pool value - All the IAssets + Collateral of the CDP.
      nStabilityPoolValue =
        V2.txOutValue op
          <> Ada.lovelaceValueOf collateralAbsorbed
          <> createAssetVal (negate burntAmount)

  let commonTx =
        mconcat
          [ spendScriptRef
              refScriptUtxo
              (cdpScript Mock.cdpParams)
              cdpOref
              Liquidate
              cdpDatum,
            spendScriptRef
              refScriptUtxoSP
              (stabilityPoolScript Mock.spParams)
              orefp
              LiquidateCDP
              ( StabilityPoolDatum
                  cdpIAsset
                  snapshot
                  epochToScaleToSum
              ),
            mintValue
              (iAssetPolicy cdpToken)
              ()
              burnValAsset,
            payToRef
              (stabilityPoolScript Mock.spParams)
              ( HashDatum $
                  StabilityPoolDatum
                    cdpIAsset
                    newSnapshot
                    newEpochToScaleToSum
              )
              nStabilityPoolValue
          ]
      tx =
        mconcat
          [ mintValue
              ( authPolicy
                  (CDPParams.cdpCreatorNft Mock.cdpCreatorParams)
                  cdpTokenName
              )
              ()
              burnValCDPToken,
            commonTx
          ]
  void $ signTx user tx >>= sendTx

runLiquidate ::
  HasCallStack =>
  Ledger.PubKeyHash ->
  LiquidateParam ->
  LiquidationVariation ->
  Run ()
runLiquidate user LiquidateParam {lAsset} variation = do
  refScriptUtxo <- getFirstRefScript (cdpScript Mock.cdpParams)
  refScriptUtxoSP <- getFirstRefScript (stabilityPoolScript Mock.spParams)
  (cdpOref, cdpO, cdpDatum@CDP {cdpIAsset, cdpMintedAmount}) <-
    findFrozenCDP Mock.cdpParams lAsset
  (orefp, op, snapshot, epochToScaleToSum) <-
    SPHelpers.findStabilityPool cdpIAsset
  let createAssetVal :: Integer -> Ledger.Value
      createAssetVal =
        Value.singleton (CDPParams.cdpAssetSymbol Mock.cdpParams) cdpIAsset
      assetAmount =
        Value.assetClassValueOf
          (V2.txOutValue op)
          (Value.assetClass (CDPParams.cdpAssetSymbol Mock.cdpParams) cdpIAsset)
      isPartial = assetAmount < cdpMintedAmount
      burntAmount :: Integer
      burntAmount = case variation of
        LiquidationFakePartial -> 1
        _ -> min cdpMintedAmount assetAmount
      collateralAvailable =
        Ada.getLovelace . Ada.fromValue . V2.txOutValue $ cdpO
      collateralForfeit =
        collateralAvailable * burntAmount `div` cdpMintedAmount
      collateralRemaining = collateralAvailable - collateralForfeit
      cdpToken = CDPParams.cdpAuthToken Mock.cdpParams
      cdpTokenName = getTokenName cdpToken
      burnValCDPToken = inv $ valueOfAssetCls (V2.txOutValue cdpO) cdpToken
      burnValAsset =
        createAssetVal (-burntAmount)
          <> case variation of
            LiquidationMoreBurns (asset, amt) ->
              Value.singleton
                (CDPParams.cdpAssetSymbol Mock.cdpParams)
                asset
                (negate amt)
            _ -> mempty
      collateralAbsorbed = case variation of
        LiquidationFakePartial -> collateralAvailable
        _ -> collateralForfeit
      (newSnapshot, newEpochToScaleToSum) = case variation of
        LiquidationSPEmpty -> (snapshot, epochToScaleToSum)
        _ ->
          liquidateHelper
            snapshot
            epochToScaleToSum
            burntAmount
            collateralAbsorbed
      nStabilityPoolValue =
        V2.txOutValue op
          <> Ada.lovelaceValueOf collateralAbsorbed
          <> createAssetVal (negate burntAmount)
  extraTestTx <- case variation of
    LiquidationStealSPAccount user2 -> do
      (aoref, ao, asnapshot) <-
        SPHelpers.findAccount (Ledger.PaymentPubKeyHash user2) lAsset
      pure
        ( spendScript
            (stabilityPoolScript Mock.spParams)
            aoref
            SpendAccount
            (AccountDatum (Ledger.PaymentPubKeyHash user2) lAsset asnapshot)
            <> payToKey user (V2.txOutValue ao)
        )
    LiquidationMoreBurns (asset, amt) -> do
      sp <-
        spend
          user
          ( Value.singleton
              (CDPParams.cdpAssetSymbol Mock.cdpParams)
              asset
              amt
          )
      pure $ userSpend sp
    _ -> pure mempty :: Run Tx
  let iAssetMintTx = case variation of
        LiquidationSPEmpty -> mempty
        _ ->
          mintValue
            (iAssetPolicy cdpToken)
            ()
            burnValAsset

  let commonTx =
        mconcat
          [ spendScriptRef
              refScriptUtxo
              (cdpScript Mock.cdpParams)
              cdpOref
              Liquidate
              cdpDatum,
            spendScriptRef
              refScriptUtxoSP
              (stabilityPoolScript Mock.spParams)
              orefp
              LiquidateCDP
              (StabilityPoolDatum cdpIAsset snapshot epochToScaleToSum),
            iAssetMintTx,
            payToRef
              (stabilityPoolScript Mock.spParams)
              ( InlineDatum $
                  StabilityPoolDatum cdpIAsset newSnapshot newEpochToScaleToSum
              )
              nStabilityPoolValue,
            extraTestTx
          ]
      tx =
        if isPartial
          then
            mconcat
              [ payToRef
                  (cdpScript Mock.cdpParams)
                  ( InlineDatum $
                      cdpDatum {cdpMintedAmount = cdpMintedAmount - assetAmount}
                  )
                  (inv burnValCDPToken <> Ada.lovelaceValueOf collateralRemaining),
                commonTx
              ]
          else
            mconcat
              [ mintValue
                  ( authPolicy
                      (CDPParams.cdpCreatorNft Mock.cdpCreatorParams)
                      cdpTokenName
                  )
                  ()
                  burnValCDPToken,
                commonTx
              ]
  void $ signTx user tx >>= sendTx

runMergeCDPs :: HasCallStack => Ledger.PubKeyHash -> MergeCDPsParam -> Run ()
runMergeCDPs user MergeCDPsParam {cdpsToMerge, mpAsset} = do
  refScriptUtxo <- getFirstRefScript (cdpScript Mock.cdpParams)
  cdpsToMergeUtxos@((mainMergeUtxoRef, _, mainMergeUtxoDatum) : restUtxos) <-
    filter
      (\(ref, _, _) -> ref `elem` cdpsToMerge)
      <$> findAllUtxosAt (cdpScript Mock.cdpParams)

  let aggregatedValue = foldMap (V2.txOutValue . view _2) cdpsToMergeUtxos
      aggregatedDatum =
        CDP
          { cdpOwner = Nothing,
            cdpIAsset = mpAsset,
            cdpMintedAmount =
              sum $
                fmap
                  (\(_, _, CDP {cdpMintedAmount}) -> cdpMintedAmount)
                  cdpsToMergeUtxos
          }
      tx =
        mconcat
          ( [ payToRef
                (cdpScript Mock.cdpParams)
                (InlineDatum aggregatedDatum)
                aggregatedValue,
              spendScriptRef
                refScriptUtxo
                (cdpScript Mock.cdpParams)
                mainMergeUtxoRef
                MergeCDPs
                mainMergeUtxoDatum
            ]
              ++ map
                ( \(oref, _, datum) ->
                    spendScriptRef
                      refScriptUtxo
                      (cdpScript Mock.cdpParams)
                      oref
                      (MergeAuxiliary mainMergeUtxoRef)
                      datum
                )
                restUtxos
          )
  void $ signTx user tx >>= sendTx

getProtocolFeePercentage :: GovParams.GovDatum -> OnChainDecimal
getProtocolFeePercentage =
  GovParams.protocolFeePercentage . GovParams.protocolParams
