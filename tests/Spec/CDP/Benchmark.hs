{-# LANGUAGE NamedFieldPuns #-}

module Spec.CDP.Benchmark (tests, initialFunds) where

import Control.Lens (view, _1)
import Control.Monad (replicateM, replicateM_, unless, void)
import Indigo.Contracts.CDP.Common hiding (CDPCreatorScript, CDPScript)
import Indigo.Contracts.CDP.Common qualified as CDPParams
import Indigo.Contracts.Governance.Gov.Common qualified as GovParams
import Indigo.Contracts.StabilityPool.Common
  ( StabilityPoolParams (accountAdjustmentFeeLovelaces, accountCreateFeeLovelaces),
    StabilityPoolSnapshot (snapshotEpoch, snapshotScale),
    accountCreateFeeLovelaces,
    initSPSnapshot,
    snapshotD,
    toSPInteger,
  )
import Indigo.Data.Decimal (OnChainDecimal (OnChainDecimal))
import Indigo.Utils.Helpers (unitValue)
import Ledger
  ( PaymentPubKeyHash (PaymentPubKeyHash),
    PubKeyHash,
    Value,
  )
import Ledger.Ada qualified as Ada
import Ledger.Ada qualified as Value
import Ledger.Value qualified as Value
import Options (SuiteOptions, setupTestBenchmarkSuite)
import Plutus.Model hiding (ada)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.IsData (FromData, ToData)
import PlutusTx.Prelude hiding (check, unless)
import Spec.CDP.Asserts (assertOpened, partialLiquidationAssert)
import Spec.CDP.Helpers (findIAsset)
import Spec.CDP.Params
  ( BurnParam (BurnParam, bAmount, bAsset, bCdpOwnerPkh),
    CloseParam (CloseParam, cAsset, cCdpOwnerPkh),
    DepositParam (DepositParam, dAmount, dAsset, dCdpOwnerPkh),
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
  )
import Spec.CDP.Script (CDPScript, cdpScript)
import Spec.CDP.Transactions
  ( AdjustVariation (..),
    CloseVariation (..),
    FreezeVariation
      ( FreezeAttachStakingCredential,
        FreezeConsumeSP,
        FreezeExtraMint,
        FreezeSucceed,
        FreezeWithStakingCredential
      ),
    IAssetType (Active, Delisted),
    LiquidationVariation
      ( LiquidationFakePartial,
        LiquidationMoreBurns,
        LiquidationSPEmpty,
        LiquidationStealSPAccount,
        LiquidationSucceed
      ),
    OpenPositionVariation
      ( ExtraMint,
        ExtraMintAndReference,
        ExtraMintWithDatum,
        IncorrectIAssetNFTUsed,
        IncorrectIAssetTokenUsed,
        IncorrectOracleNFTUsed,
        SignedByOtherUser,
        Succeed,
        WithStakingCredential
      ),
    createCloseTx,
    runBurn,
    runClose,
    runDeposit,
    runFreeze,
    runInitRefScript,
    runInitRefScriptCreator,
    runInitialize,
    runLiquidate,
    runLiquidateExploit,
    runMergeCDPs,
    runMint,
    runOpenCDPPosition,
    runWithdraw,
  )
import Spec.Collector.Transactions (runInitRefScriptCollector)
import Spec.Governance.Benchmark qualified as GovernanceBenchmark
import Spec.Governance.Transactions
  ( createDelistProposal,
    executeProposalSuccessfully,
  )
import Spec.Oracle.Benchmark qualified as OracleBenchmark
import Spec.Oracle.Helpers
  ( findOracle,
    oAssetNFTAssetClass,
    waitUntilOracleExpired,
  )
import Spec.Oracle.Script (oracleScript)
import Spec.Oracle.Transactions
  ( FeedPriceVariation (FeedPriceSucceed),
    feedPrice,
  )
import Spec.StabilityPool.Helpers (findAccount, findStabilityPool)
import Spec.StabilityPool.Params qualified as SPParams
import Spec.StabilityPool.Transactions qualified as SP
import Spec.Staking.Params (OpenStakingPositionParam (OpenStakingPositionParam))
import Spec.Staking.Transactions qualified as StakingT
import Test.Tasty (TestTree, testGroup)
import Utils.Helpers
  ( ErrorMsgContains (ErrorMsgContains),
    assertLimits,
    checkFailWithMsg,
    checkNoFail,
    findUniqueUtxo,
    findUtxos,
    minLovelacesPerUtxo,
  )
import Utils.Mock qualified as Mock
import Prelude (Int, Integral (div), String)
import Prelude qualified as P

tests :: SuiteOptions -> MockConfig -> TestTree
tests suiteOpts cfg =
  let initialOpenPosParam =
        OpenPositionParam
          { opAsset = iBTCTokenName,
            opCollateralAmount = 15_000_000,
            opMintedAmount = 3_000_000
          }
      initialDepositParam user =
        DepositParam
          { dCdpOwnerPkh = PaymentPubKeyHash user,
            dAsset = iBTCTokenName,
            dAmount = 2_000_000
          }
      initialWithdrawParam user =
        WithdrawParam
          { wCdpOwnerPkh = PaymentPubKeyHash user,
            wAsset = iBTCTokenName,
            wAmount = 2_000_000
          }
      initialMintParam user =
        MintParam
          { mCdpOwnerPkh = PaymentPubKeyHash user,
            mAsset = iBTCTokenName,
            mAmount = 1
          }
      initialBurnParam user =
        BurnParam
          { bCdpOwnerPkh = PaymentPubKeyHash user,
            bAsset = iBTCTokenName,
            bAmount = 1
          }
      initialCloseParam user =
        CloseParam
          { cCdpOwnerPkh = PaymentPubKeyHash user,
            cAsset = iBTCTokenName
          }
      initialFreezeParam user =
        FreezeParam {fCdpOwnerPkh = user, fAsset = iBTCTokenName}
      initialLiquidateParam = LiquidateParam {lAsset = iBTCTokenName}
      specs =
        [ testGroup
            "Open position"
            [ check
                "Open position correctly"
                ( do
                    runInitialize initialOracles 1
                    void $ openPos' initialOpenPosParam Succeed
                    assertOpened
                      1
                      (opCollateralAmount initialOpenPosParam)
                      (P.length initialOracles)
                ),
              checkNoFail
                cfg
                ( initialFunds
                    <> Value.singleton
                      (CDPParams.cdpAssetSymbol Mock.cdpParams)
                      iBTCTokenName
                      5_000_000
                )
                "Open position with 0 minted tokens"
                ( do
                    runInitialize initialOracles 1
                    void
                      ( openPos'
                          initialOpenPosParam
                          $ ExtraMintWithDatum (-3_000_000)
                      )
                    assertOpened
                      1
                      (opCollateralAmount initialOpenPosParam)
                      (P.length initialOracles)
                ),
              checkFailWithMsg'
                "Open position with oracle expired fails"
                (ErrorMsgContains "Oracle cannot be expired")
                ( do
                    runInitialize initialOracles 1
                    waitUntilOracleExpired
                    void $ openPos' initialOpenPosParam Succeed
                ),
              check
                "Open position with staking credential succeeds"
                ( do
                    runInitialize initialOracles 1
                    void (openPos' initialOpenPosParam WithStakingCredential)
                ),
              checkFailWithMsg'
                "Open position cannot be owned by another user"
                (ErrorMsgContains "The transaction is not signed by cdp owner")
                ( do
                    runInitialize initialOracles 1
                    void $ openPos' initialOpenPosParam SignedByOtherUser
                ),
              let fakeIAssetNft = fakeCoin (FakeCoin "fake_iasset_token")
               in checkFailWithMsg
                    cfg
                    (initialFunds <> unitValue fakeIAssetNft)
                    "Open Position with incorrect iAsset NFT fails"
                    (ErrorMsgContains "Expected exactly one reference input")
                    ( do
                        runInitialize initialOracles 1
                        sendFakeIAssetToCDP
                          iBTCTokenName
                          ( Value.lovelaceValueOf minLovelacesPerUtxo
                              <> unitValue fakeIAssetNft
                          )
                        void
                          $ openPos'
                            initialOpenPosParam
                          $ IncorrectIAssetNFTUsed fakeIAssetNft
                    ),
              checkFailWithMsg
                cfg
                initialFunds
                "Open position with a different iAsset fails"
                (ErrorMsgContains "Minted value is invalid")
                ( do
                    let oracles =
                          [ (iBTCTokenName, OnChainDecimal 1_000_000, Active),
                            (iETHTokenName, OnChainDecimal 1_000_000, Active)
                          ]
                    runInitialize oracles 1
                    void
                      $ openPos'
                        initialOpenPosParam
                      $ IncorrectIAssetTokenUsed iETHTokenName
                ),
              let fakeOracleNft = oAssetNFTAssetClass "fake_iBTC_oracle_nft"
               in checkFailWithMsg
                    cfg
                    (initialFunds <> unitValue fakeOracleNft)
                    "Open Position with incorrect Oracle NFT fails"
                    (ErrorMsgContains "Expected exactly one reference input")
                    ( do
                        runInitialize initialOracles 1
                        param <- Mock.oracleParams
                        (_, _, datum) <- findOracle iBTCTokenName
                        sendToScript
                          (oracleScript param)
                          datum
                          ( Value.lovelaceValueOf minLovelacesPerUtxo
                              <> unitValue fakeOracleNft
                          )
                        void
                          $ openPos'
                            initialOpenPosParam
                          $ IncorrectOracleNFTUsed fakeOracleNft
                    ),
              checkFailWithMsg'
                "Open Position of delisted iAsset fails"
                (ErrorMsgContains "Cannot open a CDP of a delisted asset")
                ( do
                    runInitialize
                      [(iBTCTokenName, OnChainDecimal 1_000_000, Delisted)]
                      1
                    void (openPos' initialOpenPosParam Succeed)
                ),
              checkFailWithMsg'
                "Open an under-collateralized position fails"
                (ErrorMsgContains "Undercollaterized CDP")
                ( do
                    runInitialize initialOracles 1
                    void $
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 11_000_000,
                            opMintedAmount = 11_000_000
                          }
                        Succeed
                ),
              checkFailWithMsg'
                "Open a CDP under min collateral in lovelace"
                (ErrorMsgContains "Must provide more than minCollateralInLovelace")
                ( do
                    runInitialize initialOracles 1
                    void $
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 9_000_000,
                            opMintedAmount = 3_000_000
                          }
                        Succeed
                ),
              checkFailWithMsg'
                "Open Position with multiple minted iAssets fails"
                (ErrorMsgContains "Minted value is invalid")
                ( do
                    runInitialize initialOracles 1
                    let opMintedAmount = 1_000_000
                        extraValue =
                          Value.singleton
                            (CDPParams.cdpAssetSymbol Mock.cdpParams)
                            iETHTokenName
                            opMintedAmount
                    void
                      $ openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 11_000_000,
                            opMintedAmount
                          }
                      $ ExtraMint extraValue
                ),
              checkFailWithMsg
                cfg
                initialFunds
                "Open Position with multiple minted and refer. iAssets fails"
                (ErrorMsgContains "Expected exactly one reference input")
                ( do
                    runInitialize
                      [ (iBTCTokenName, OnChainDecimal 1_000_000, Active),
                        (iETHTokenName, OnChainDecimal 1_000_000, Active)
                      ]
                      1
                    let opMintedAmount = 1_000_000
                    void . openPos' initialOpenPosParam $
                      ExtraMintAndReference iETHTokenName opMintedAmount
                ),
              checkFailWithMsg'
                "Open Position with additional amount of iAsset fails"
                (ErrorMsgContains "Minted value is invalid")
                ( do
                    runInitialize initialOracles 1
                    let opMintedAmount = 1_000_000
                        extraValue =
                          Value.singleton
                            (CDPParams.cdpAssetSymbol Mock.cdpParams)
                            iBTCTokenName
                            100
                    void
                      $ openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 3_000_000,
                            opMintedAmount
                          }
                      $ ExtraMint extraValue
                ),
              checkFailWithMsg
                cfg
                ( initialFunds
                    <> Value.singleton
                      (CDPParams.cdpAssetSymbol Mock.cdpParams)
                      iBTCTokenName
                      5_000_000
                )
                "Open Position with negative amount of iAsset fails"
                (ErrorMsgContains "Negative minted amount")
                ( do
                    runInitialize initialOracles 1
                    void
                      $ openPos'
                        initialOpenPosParam
                      $ ExtraMintWithDatum (-4_000_000)
                )
            ],
          testGroup
            "Adjust position"
            [ check
                "deposit succeeds"
                ( do
                    runInitialize initialOracles 1
                    openPos' initialOpenPosParam Succeed
                      >>= deposit' initialDepositParam AdjustSucceed
                ),
              check
                "deposit succeeds with StakingCredential"
                ( do
                    runInitialize initialOracles 1
                    openPos' initialOpenPosParam WithStakingCredential
                      >>= deposit' initialDepositParam AdjustWithStakingCredential
                ),
              check
                "withdraw succeeds"
                ( do
                    runInitialize initialOracles 1
                    openPos' initialOpenPosParam Succeed
                      >>= withdraw' initialWithdrawParam
                ),
              check
                "mint succeeds"
                ( do
                    runInitialize initialOracles 1
                    openPos' initialOpenPosParam Succeed
                      >>= mint' initialMintParam
                ),
              check
                "burn succeeds"
                ( do
                    runInitialize initialOracles 1
                    openPos' initialOpenPosParam Succeed
                      >>= burn' initialBurnParam
                ),
              check
                "Deposit after oracle expired succeeds"
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    waitUntilOracleExpired
                    deposit' initialDepositParam AdjustSucceed user
                ),
              checkFailWithMsg'
                "Withdrawal after oracle expired fails"
                (ErrorMsgContains "Oracle cannot be expired")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    waitUntilOracleExpired
                    withdraw' initialWithdrawParam user
                ),
              checkFailWithMsg'
                "Withdraw under minCollateralInLovelace fails"
                (ErrorMsgContains "Must provide more than minCollateralInLovelace")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    runWithdraw
                      WithdrawParam
                        { wCdpOwnerPkh = PaymentPubKeyHash user,
                          wAsset = iBTCTokenName,
                          wAmount = 5_000_001
                        }
                ),
              checkFailWithMsg'
                "Mint after oracle expired fails"
                (ErrorMsgContains "Oracle cannot be expired")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    waitUntilOracleExpired
                    mint' initialMintParam user
                ),
              check
                "Burn after oracle expired succeeds"
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    waitUntilOracleExpired
                    burn' initialBurnParam user
                ),
              let fakeIAssetNft =
                    fakeCoin (FakeCoin $ Value.unTokenName fakeIAssetName)
                  fakeIAssetName = "fake_iasset_token"
               in checkFailWithMsg
                    cfg
                    (initialFunds <> Value.assetClassValue fakeIAssetNft 2)
                    "Deposit collateral with incorrect iAsset NFT fails"
                    (ErrorMsgContains "Incorrect iAsset input")
                    ( do
                        let oracles =
                              [ ( iBTCTokenName,
                                  OnChainDecimal 1_000_000,
                                  Active
                                ),
                                ( fakeIAssetName,
                                  OnChainDecimal 1_000_000,
                                  Active
                                )
                              ]
                            fakeIAssetVal =
                              Value.lovelaceValueOf minLovelacesPerUtxo
                                <> unitValue fakeIAssetNft
                        runInitialize oracles 1
                        (_, _, iAsset) <- findIAsset iBTCTokenName
                        user <- openPos' initialOpenPosParam Succeed
                        admin <- getMainUser
                        withSpend
                          admin
                          fakeIAssetVal
                          ( \sp -> do
                              let tx =
                                    P.mconcat
                                      [ userSpend sp,
                                        payToScript
                                          (cdpScript Mock.cdpParams)
                                          (InlineDatum (IAssetDatum iAsset))
                                          fakeIAssetVal
                                      ]
                              void $ signTx admin tx >>= sendTx
                          )
                        runDeposit
                          (initialDepositParam user)
                          (IncorrectIAssetNFTUsedToAdjust fakeIAssetName)
                    ),
              checkFailWithMsg'
                "Deposit collateral cannot be used to steal iAsset from SP"
                (ErrorMsgContains "CDP input must be liquidated")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    SP.runCreate
                      user
                      (SPParams.CreateParam iBTCTokenName 3_000_000)
                      SP.OpenSucceed
                    runDeposit
                      (initialDepositParam user)
                      (SpendStabilityPoolOutput iBTCTokenName)
                ),
              checkFailWithMsg'
                "Deposit additional collateral to CDP not owned by user"
                (ErrorMsgContains "The transaction is not signed by cdp owner")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    otherUser <- newUser mempty
                    runDeposit
                      (initialDepositParam user)
                      (AdjustSignedByOtherUser otherUser)
                )
            ],
          testGroup
            "Close positiion"
            [ check
                "Correctly closes CDP."
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    close' initialCloseParam CloseSucceed user
                ),
              check
                "Correctly closes CDP with staking credential."
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam WithStakingCredential
                    close' initialCloseParam CloseWithStakingCredential user
                ),
              check
                "Closes CDP with contention on collector"
                closeWithContention',
              check
                "Close after oracle expired succeeds"
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    waitUntilOracleExpired
                    close' initialCloseParam CloseSucceed user
                ),
              checkFailWithMsg'
                "Close CDP of another user fails."
                (ErrorMsgContains "The transaction is not signed by cdp owner")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    otherUser <- newUser mempty
                    close'
                      initialCloseParam
                      (CloseSignedByOtherUser otherUser)
                      user
                ),
              checkFailWithMsg'
                "Close CDP cannot burn more iAsset than in CDP."
                (ErrorMsgContains "Burned value is invalid")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    user2 <- openPos' initialOpenPosParam Succeed
                    sendValue
                      user2
                      ( Value.singleton
                          (CDPParams.cdpAssetSymbol Mock.cdpParams)
                          iBTCTokenName
                          1
                      )
                      user
                    close' initialCloseParam BurnMoreAsset user
                ),
              checkFailWithMsg'
                "Close CDP cannot burn less iAsset than in CDP."
                (ErrorMsgContains "Burned value is invalid")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    close' initialCloseParam BurnLessAsset user
                ),
              checkFailWithMsg'
                "Close CDP and not sending protocol fee fails."
                (ErrorMsgContains "Must spend 1 input from collector script")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    close' initialCloseParam NoProtocolFee user
                ),
              checkFailWithMsg'
                "Close CDP and sending incorrect protocol fee fails."
                (ErrorMsgContains "Must pay protocol fee correctly")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    close'
                      initialCloseParam
                      (IncorrectProtocolFee $ OnChainDecimal 500_000)
                      user
                ),
              check
                "Close CDP of delisted asset succeeds."
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    admin <- getMainUser
                    StakingT.runOpenStakingPos
                      admin
                      (OpenStakingPositionParam 1_000_000_000)
                      StakingT.OpenSucceed
                    pId <- createDelistProposal iBTCTokenName
                    executeProposalSuccessfully pId
                    close' initialCloseParam CloseSucceed user
                )
            ],
          testGroup
            "Liquidate position"
            [ check
                "freeze succeeds"
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                ),
              check
                "freeze succeeds of CDP with staking credential, removing credential"
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam WithStakingCredential
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeWithStakingCredential
                ),
              checkFailWithMsg'
                "freeze fails when attaching staking credential to output"
                (ErrorMsgContains "CDP output is incorrect")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam WithStakingCredential
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeAttachStakingCredential
                ),
              check
                "freeze after oracle expired succeeds"
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    waitUntilOracleExpired
                    runFreeze (initialFreezeParam user) FreezeSucceed
                ),
              checkFailWithMsg'
                "freeze fails when cdp overcollateralized"
                (ErrorMsgContains "CDP still overcollaterized")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                ),
              checkFailWithMsg'
                "freeze fails to mint extra iAsset"
                (ErrorMsgContains "Can not mint any token")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    runFreeze
                      (initialFreezeParam user)
                      ( FreezeExtraMint $
                          Value.singleton
                            (CDPParams.cdpAssetSymbol Mock.cdpParams)
                            iBTCTokenName
                            100_000
                      )
                ),
              checkFailWithMsg'
                "full liquidation doesnt allow more iAsset to be burned than mintedAsset"
                (ErrorMsgContains "Expected burn all cdp auth tokens in cdp input and all minted iAssets")
                ( do
                    admin <- getMainUser
                    runInitialize
                      [(iBTCTokenName, OnChainDecimal 100_000, Active)]
                      1
                    user1 <- openPos' initialOpenPosParam Succeed
                    user2 <- openPos' initialOpenPosParam Succeed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze
                      (initialFreezeParam user1)
                      FreezeSucceed

                    SP.runCreate
                      user1
                      ( SPParams.CreateParam
                          iBTCTokenName
                          3_000_000
                      )
                      SP.OpenSucceed
                    SP.runCreate
                      user2
                      ( SPParams.CreateParam
                          iBTCTokenName
                          3_000_000
                      )
                      SP.OpenSucceed

                    -- Now the total IBTC in the stability pool is 6_000_000
                    runLiquidateExploit admin initialLiquidateParam
                ),
              checkFailWithMsg'
                "freeze consuming SP fails"
                (ErrorMsgContains "CDP input must be liquidated")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    SP.runCreate
                      user
                      (SPParams.CreateParam iBTCTokenName 3_000_000)
                      SP.OpenSucceed
                    runFreeze (initialFreezeParam user) FreezeConsumeSP
                ),
              check
                "liquidate succeeds"
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    SP.runCreate
                      user
                      (SPParams.CreateParam iBTCTokenName 3_000_000)
                      SP.OpenSucceed
                    runLiquidate user initialLiquidateParam LiquidationSucceed
                ),
              check
                "Partial liquidation has to increase epoch"
                ( do
                    runInitialize initialOracles 1
                    user1 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 60_000_000,
                            opMintedAmount = 20_000_000
                          }
                        Succeed
                    user2 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 80_000_000,
                            opMintedAmount = 40_000_000
                          }
                        Succeed
                    user3 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 30_000_000,
                            opMintedAmount = 20_000_000
                          }
                        Succeed
                    user4 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 45_000_000,
                            opMintedAmount = 30_000_000
                          }
                        Succeed

                    SP.runCreate
                      user1
                      (SPParams.CreateParam iBTCTokenName 20_000_000)
                      SP.OpenSucceed
                    SP.runCreate
                      user2
                      (SPParams.CreateParam iBTCTokenName 40_000_000)
                      SP.OpenSucceed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 2_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user3) FreezeSucceed
                    runLiquidate user3 initialLiquidateParam LiquidationSucceed

                    SP.runClose user1 (SPParams.CloseParam iBTCTokenName)

                    -- Partial liquidation
                    runFreeze (initialFreezeParam user4) FreezeSucceed
                    runLiquidate user4 initialLiquidateParam LiquidationSucceed

                    (_, _, snap, _) <- findStabilityPool iBTCTokenName
                    unless
                      (snap P.== initSPSnapshot {snapshotEpoch = 1})
                      (logError "Epoch didn't increase")
                ),
              check
                "liquidation rewards distributed equally among 2 depositors + SP deposit fee distribution checks"
                ( do
                    runInitialize initialOracles 1
                    user <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 40_000_000,
                            opMintedAmount = 20_000_000
                          }
                        Succeed
                    depositor1 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 30_000_000,
                            opMintedAmount = 10_000_000
                          }
                        Succeed
                    depositor2 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 30_000_000,
                            opMintedAmount = 10_000_000
                          }
                        Succeed
                    SP.runCreate depositor1 (SPParams.CreateParam iBTCTokenName 10_000_000) SP.OpenSucceed
                    SP.runCreate depositor2 (SPParams.CreateParam iBTCTokenName 10_000_000) SP.OpenSucceed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 2_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    runLiquidate user initialLiquidateParam LiquidationSucceed

                    -- Depositor1 gets 1/2 of the liquidation collateral +
                    -- all of his deposit fee +
                    -- 1/2 of depositor2 fee
                    checkBalance
                      ( let adaReward =
                              (accountCreateFeeLovelaces Mock.spParams `div` 2) * 3
                                + 20_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor1
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose depositor1 (SPParams.CloseParam iBTCTokenName)
                      )
                    -- Depositor2 gets 1/2 of the liquidation collateral +
                    -- 1/2 of his depositor fee
                    checkBalance
                      ( let adaReward =
                              accountCreateFeeLovelaces Mock.spParams `div` 2
                                + 20_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor2
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose depositor2 (SPParams.CloseParam iBTCTokenName)
                      )
                ),
              check
                "liquidation rewards distributed proportionally among 3 depositors + SP deposit fee distribution checks"
                ( do
                    runInitialize initialOracles 1
                    user <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 100_000_000,
                            opMintedAmount = 50_000_000
                          }
                        Succeed
                    depositor1 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 45_000_000,
                            opMintedAmount = 15_000_000
                          }
                        Succeed
                    depositor2 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 30_000_000,
                            opMintedAmount = 10_000_000
                          }
                        Succeed
                    depositor3 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 75_000_000,
                            opMintedAmount = 25_000_000
                          }
                        Succeed

                    SP.runCreate depositor1 (SPParams.CreateParam iBTCTokenName 15_000_000) SP.OpenSucceed
                    SP.runCreate depositor2 (SPParams.CreateParam iBTCTokenName 10_000_000) SP.OpenSucceed
                    SP.runCreate depositor3 (SPParams.CreateParam iBTCTokenName 25_000_000) SP.OpenSucceed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 2_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    runLiquidate user initialLiquidateParam LiquidationSucceed

                    -- Depositor1 gets 3/10 of the liquidation collateral +
                    -- all of his deposit fee +
                    -- 3/5 of depositor2 fee +
                    -- 3/10 of depositor3 fee
                    checkBalance
                      ( let adaReward =
                              accountCreateFeeLovelaces Mock.spParams
                                + (accountCreateFeeLovelaces Mock.spParams `div` 5) * 3
                                + (accountCreateFeeLovelaces Mock.spParams `div` 10) * 3
                                + 30_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor1
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose depositor1 (SPParams.CloseParam iBTCTokenName)
                      )
                    -- Depositor2 gets 2/10 of the liquidation collateral +
                    -- 2/5 of his depositor fee +
                    -- 2/10 of depositor 3 fee
                    checkBalance
                      ( let adaReward =
                              (accountCreateFeeLovelaces Mock.spParams `div` 5) * 2
                                + (accountCreateFeeLovelaces Mock.spParams `div` 10) * 2
                                + 20_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor2
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose depositor2 (SPParams.CloseParam iBTCTokenName)
                      )
                    -- Depositor2 gets 5/10 of the liquidation collateral +
                    -- 5/10 of his fee
                    checkBalance
                      ( let adaReward =
                              (accountCreateFeeLovelaces Mock.spParams `div` 10) * 5
                                + 50_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor3
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose depositor3 (SPParams.CloseParam iBTCTokenName)
                      )
                ),
              check
                "liquidation rewards distributed proportionally among 2 depositors + SP deposit/adjust fee distribution checks"
                ( do
                    runInitialize initialOracles 1
                    user <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 80_000_000,
                            opMintedAmount = 40_000_000
                          }
                        Succeed
                    depositor1 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 90_000_000,
                            opMintedAmount = 30_000_000
                          }
                        Succeed
                    depositor2 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 30_000_000,
                            opMintedAmount = 10_000_000
                          }
                        Succeed
                    SP.runCreate depositor1 (SPParams.CreateParam iBTCTokenName 15_000_000) SP.OpenSucceed
                    SP.runCreate depositor2 (SPParams.CreateParam iBTCTokenName 10_000_000) SP.OpenSucceed

                    -- Gets all of his deposit fee +
                    -- 3/5 of depositor2 deposit fee
                    checkBalance
                      ( let adaReward =
                              accountCreateFeeLovelaces Mock.spParams
                                + (accountCreateFeeLovelaces Mock.spParams `div` 5) * 3
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor1
                              ( Ada.lovelaceValueOf (adaReward - fee)
                                  <> Value.assetClassValue
                                    (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
                                    (-15_000_000)
                              )
                      )
                      (SP.runDeposit depositor1 (SPParams.DepositParam iBTCTokenName 15_000_000))

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 2_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    runLiquidate user initialLiquidateParam LiquidationSucceed

                    -- Depositor1 gets 3/4 of the liquidation collateral +
                    -- 3/4 of his adjustment fee +
                    checkBalance
                      ( let adaReward =
                              (accountAdjustmentFeeLovelaces Mock.spParams `div` 4) * 3
                                + 60_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor1
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose depositor1 (SPParams.CloseParam iBTCTokenName)
                      )
                    -- Depositor1 gets 1/4 of the liquidation collateral +
                    -- 1/4 of depositor1 adjustment fee +
                    -- 2/5 of his deposit fee
                    checkBalance
                      ( let adaReward =
                              (accountAdjustmentFeeLovelaces Mock.spParams `div` 4)
                                + (accountCreateFeeLovelaces Mock.spParams `div` 5) * 2
                                + 20_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor2
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose depositor2 (SPParams.CloseParam iBTCTokenName)
                      )
                ),
              check
                "liquidation emptying stability pool rewards distributed proportionally among 2 depositors + SP deposit fee distribution checks"
                ( do
                    runInitialize initialOracles 1
                    user <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 40_000_000,
                            opMintedAmount = 20_000_000
                          }
                        Succeed
                    depositor1 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 30_000_000,
                            opMintedAmount = 20_000_000
                          }
                        Succeed
                    depositor2 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 30_000_000,
                            opMintedAmount = 10_000_000
                          }
                        Succeed
                    SP.runCreate depositor1 (SPParams.CreateParam iBTCTokenName 10_000_000) SP.OpenSucceed
                    SP.runCreate depositor2 (SPParams.CreateParam iBTCTokenName 10_000_000) SP.OpenSucceed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 2_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    runLiquidate user initialLiquidateParam LiquidationSucceed

                    (_, _, snapshot, _) <- findStabilityPool iBTCTokenName
                    unless
                      (snapshot P.== initSPSnapshot {snapshotEpoch = 1})
                      (logError "Epoch didn't get incremented when SP emptied")

                    -- All of his deposit fee +
                    -- 1/2 of depositor2 deposit fee +
                    -- 1/2 of liquidation collateral
                    checkBalance
                      ( let adaReward =
                              accountCreateFeeLovelaces Mock.spParams
                                + (accountCreateFeeLovelaces Mock.spParams `div` 2)
                                + 20_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor1
                              ( Ada.lovelaceValueOf (adaReward - fee)
                                  <> Value.assetClassValue
                                    (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
                                    (-10_000_000)
                              )
                      )
                      (SP.runDeposit depositor1 (SPParams.DepositParam iBTCTokenName 10_000_000))

                    -- Depositor1 gets all of his adjustment fee
                    -- (since depositor2 already has 0 deposit after SP emptying liquidation)
                    checkBalance
                      ( let adaReward =
                              accountAdjustmentFeeLovelaces Mock.spParams
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor1
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                                  <> Value.assetClassValue
                                    (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
                                    10_000_000
                              )
                      )
                      ( SP.runClose depositor1 (SPParams.CloseParam iBTCTokenName)
                      )
                    -- Depositor2 gets 1/2 of the liquidation collateral +
                    -- 1/2 of his depositor fee
                    checkBalance
                      ( let adaReward =
                              (accountCreateFeeLovelaces Mock.spParams `div` 2)
                                + 20_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor2
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose depositor2 (SPParams.CloseParam iBTCTokenName)
                      )
                ),
              check
                "partial liquidate rewards check + SP deposit fee check"
                ( do
                    runInitialize initialOracles 1
                    user <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 80_000_000,
                            opMintedAmount = 40_000_000
                          }
                        Succeed
                    depositor1 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 30_000_000,
                            opMintedAmount = 10_000_000
                          }
                        Succeed
                    depositor2 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 30_000_000,
                            opMintedAmount = 10_000_000
                          }
                        Succeed

                    SP.runCreate depositor1 (SPParams.CreateParam iBTCTokenName 10_000_000) SP.OpenSucceed
                    SP.runCreate depositor2 (SPParams.CreateParam iBTCTokenName 10_000_000) SP.OpenSucceed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 2_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    runLiquidate user initialLiquidateParam LiquidationSucceed

                    partialLiquidationAssert 20_000_000 iBTCTokenName

                    checkBalance
                      ( let adaReward =
                              accountCreateFeeLovelaces Mock.spParams
                                + (accountCreateFeeLovelaces Mock.spParams `div` 2)
                                + 20_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor1
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose depositor1 (SPParams.CloseParam iBTCTokenName)
                      )
                    checkBalance
                      ( let adaReward =
                              (accountCreateFeeLovelaces Mock.spParams `div` 2)
                                + 20_000_000
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              depositor2
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose depositor2 (SPParams.CloseParam iBTCTokenName)
                      )
                ),
              check
                "partial liquidate succeeds"
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    SP.runCreate
                      user
                      (SPParams.CreateParam iBTCTokenName 2_000_000)
                      SP.OpenSucceed
                    runLiquidate user initialLiquidateParam LiquidationSucceed

                    partialLiquidationAssert 1_000_000 iBTCTokenName
                ),
              checkFailWithMsg'
                "Partial liquidation when SP empty will fail"
                -- division by zero in the liquidateHelper should appear since snapshotD is 0
                (ErrorMsgContains "divide")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    runLiquidate user initialLiquidateParam LiquidationSPEmpty
                ),
              check
                "liquidate after oracle expired succeeds"
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    waitUntilOracleExpired
                    SP.runCreate
                      user
                      (SPParams.CreateParam iBTCTokenName 3_000_000)
                      SP.OpenSucceed
                    runLiquidate user initialLiquidateParam LiquidationSucceed
                ),
              check
                "merge succeeds"
                ( do
                    admin <- getMainUser
                    runInitialize initialOracles 1
                    user1 <- openPos' initialOpenPosParam Succeed
                    user2 <- openPos' initialOpenPosParam Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user1) FreezeSucceed
                    runFreeze (initialFreezeParam user2) FreezeSucceed
                    mergeAll admin iBTCTokenName
                ),
              check
                "merge succeeds with one CDP with staking credential"
                ( do
                    admin <- getMainUser
                    runInitialize initialOracles 1
                    user1 <- openPos' initialOpenPosParam Succeed
                    user2 <- openPos' initialOpenPosParam WithStakingCredential
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user1) FreezeSucceed
                    runFreeze (initialFreezeParam user2) FreezeWithStakingCredential
                    mergeAll admin iBTCTokenName
                ),
              check
                "2 subsequent CDP merges succeed"
                ( do
                    admin <- getMainUser
                    runInitialize initialOracles 1
                    user1 <- openPos' initialOpenPosParam Succeed
                    user2 <- openPos' initialOpenPosParam Succeed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user1) FreezeSucceed
                    runFreeze (initialFreezeParam user2) FreezeSucceed
                    mergeAll admin iBTCTokenName

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000)
                      FeedPriceSucceed
                    user3 <- openPos' initialOpenPosParam Succeed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user3) FreezeSucceed
                    mergeAll admin iBTCTokenName
                ),
              check
                "liquidate after merge succeeds"
                ( do
                    admin <- getMainUser
                    runInitialize initialOracles 1
                    user1 <- openPos' initialOpenPosParam Succeed
                    user2 <- openPos' initialOpenPosParam Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user1) FreezeSucceed
                    runFreeze (initialFreezeParam user2) FreezeSucceed
                    mergeAll admin iBTCTokenName

                    SP.runCreate
                      user1
                      (SPParams.CreateParam iBTCTokenName 3_000_000)
                      SP.OpenSucceed
                    SP.runCreate
                      user2
                      (SPParams.CreateParam iBTCTokenName 3_000_000)
                      SP.OpenSucceed
                    runLiquidate admin initialLiquidateParam LiquidationSucceed
                ),
              check
                "partial liquidate after merge succeeds"
                ( do
                    admin <- getMainUser
                    runInitialize initialOracles 1
                    user1 <- openPos' initialOpenPosParam Succeed
                    user2 <- openPos' initialOpenPosParam Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user1) FreezeSucceed
                    runFreeze (initialFreezeParam user2) FreezeSucceed
                    mergeAll admin iBTCTokenName

                    SP.runCreate
                      user1
                      (SPParams.CreateParam iBTCTokenName 3_000_000)
                      SP.OpenSucceed
                    SP.runCreate
                      user2
                      (SPParams.CreateParam iBTCTokenName 2_000_000)
                      SP.OpenSucceed

                    runLiquidate admin initialLiquidateParam LiquidationSucceed

                    -- Asserts - when partial liquidation succeeds
                    -- CDP UTXO should still exist
                    let condition :: TxBox CDPScript -> Bool
                        condition
                          TxBox
                            { txBoxDatum =
                                CDP
                                  { cdpOwner = Nothing,
                                    cdpIAsset,
                                    cdpMintedAmount = 1_000_000
                                  }
                            } = cdpIAsset P.== iBTCTokenName
                        condition _ = False
                    (_, o, _) <-
                      findUniqueUtxo
                        (cdpScript Mock.cdpParams)
                        condition
                    unless
                      ( Value.assetClassValueOf
                          (V2.txOutValue o)
                          (CDPParams.cdpAuthToken Mock.cdpParams)
                          P.== 2
                      )
                      (logError "CDP output has correct amount of auth tokens")
                ),
              check
                "liquidation emptying stability pool succeeds"
                ( do
                    admin <- getMainUser
                    let liquidationCollateral = 75_000_000
                    runInitialize initialOracles 1

                    userDepositor <- createSPAccount 200_000_000 100_000_000

                    user1 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = liquidationCollateral,
                            opMintedAmount = 50_000_000
                          }
                        Succeed
                    user2 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = liquidationCollateral,
                            opMintedAmount = 50_000_000
                          }
                        Succeed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_100_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user1) FreezeSucceed
                    runLiquidate admin initialLiquidateParam LiquidationSucceed

                    runFreeze (initialFreezeParam user2) FreezeSucceed
                    runLiquidate admin initialLiquidateParam LiquidationSucceed

                    checkBalance
                      ( let adaReward =
                              liquidationCollateral * 2
                                + accountCreateFeeLovelaces Mock.spParams
                            fee =
                              protocolFee
                                (GovParams.protocolFeePercentage Mock.protocolParams)
                                adaReward
                                0
                         in owns
                              userDepositor
                              ( Ada.lovelaceValueOf minLovelacesPerUtxo
                                  <> Ada.lovelaceValueOf (adaReward - fee)
                              )
                      )
                      ( SP.runClose
                          userDepositor
                          SPParams.CloseParam {SPParams.clTokenName = iBTCTokenName}
                      )

                    (_, _, snapshot, _) <- findStabilityPool iBTCTokenName
                    unless
                      (snapshot P.== initSPSnapshot {snapshotEpoch = 1})
                      (logError "Epoch didn't get incremented when SP emptied")
                ),
              checkFailWithMsg'
                "liquidation can't steal an SP account"
                (ErrorMsgContains "More than one SP input")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    user2 <- openPos' initialOpenPosParam Succeed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed

                    SP.runCreate
                      user2
                      (SPParams.CreateParam iBTCTokenName 3_000_000)
                      SP.OpenSucceed
                    runLiquidate
                      user
                      initialLiquidateParam
                      (LiquidationStealSPAccount user2)
                ),
              checkFailWithMsg'
                "faking partial liquidation fails"
                (ErrorMsgContains "CDP output is incorrect")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    SP.runCreate
                      user
                      (SPParams.CreateParam iBTCTokenName 3_000_000)
                      SP.OpenSucceed
                    runLiquidate
                      user
                      initialLiquidateParam
                      LiquidationFakePartial
                ),
              checkFailWithMsg'
                "when account's snapshotD below 1, nothing can be withdrawn"
                (ErrorMsgContains "User cannot have negative balance")
                ( do
                    runInitialize initialOracles 1

                    feedPrice iBTCTokenName 10_000_000 FeedPriceSucceed

                    depositor1 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 1_000_000_000,
                            opMintedAmount = 1
                          }
                        Succeed
                    depositor2 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 1_000_000_000,
                            opMintedAmount = 3
                          }
                        Succeed

                    SP.runCreate
                      depositor1
                      (SPParams.CreateParam iBTCTokenName 1)
                      SP.OpenSucceed
                    SP.runCreate
                      depositor2
                      (SPParams.CreateParam iBTCTokenName 3)
                      SP.OpenSucceed

                    user2 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 30_000_000,
                            opMintedAmount = 2
                          }
                        Succeed

                    feedPrice iBTCTokenName 10_000_001 FeedPriceSucceed
                    runFreeze (initialFreezeParam user2) FreezeSucceed
                    runLiquidate user2 initialLiquidateParam LiquidationSucceed

                    (_, _, accountSnap) <-
                      findAccount
                        (Ledger.PaymentPubKeyHash depositor1)
                        iBTCTokenName

                    unless
                      (snapshotD accountSnap P.< toSPInteger 1)
                      (logError "snapshotD of depositor1's account is not below 1")

                    SP.runWithdraw
                      depositor1
                      (SPParams.WithdrawParam iBTCTokenName 1)
                ),
              check
                "more complex liquidate use case"
                ( do
                    runInitialize initialOracles 1

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 100_000_000)
                      FeedPriceSucceed

                    user1 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 1_000_000_000,
                            opMintedAmount = 6_250_000
                          }
                        Succeed

                    SP.runCreate
                      user1
                      (SPParams.CreateParam iBTCTokenName 1_000_002)
                      SP.OpenSucceed

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 10_000_000_000)
                      FeedPriceSucceed

                    runFreeze (initialFreezeParam user1) FreezeSucceed
                    runLiquidate user1 initialLiquidateParam LiquidationSucceed

                    SP.runClose
                      user1
                      SPParams.CloseParam {SPParams.clTokenName = iBTCTokenName}

                    SP.runCreate
                      user1
                      (SPParams.CreateParam iBTCTokenName 5_249_998)
                      SP.OpenSucceed

                    runLiquidate user1 initialLiquidateParam LiquidationSucceed

                    SP.runClose
                      user1
                      SPParams.CloseParam {SPParams.clTokenName = iBTCTokenName}

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 100_000_000)
                      FeedPriceSucceed

                    user2 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 500_000_000,
                            opMintedAmount = 3_125_000
                          }
                        Succeed

                    user3 <-
                      openPos'
                        OpenPositionParam
                          { opAsset = iBTCTokenName,
                            opCollateralAmount = 600_000_000,
                            opMintedAmount = 3_750_000
                          }
                        Succeed

                    SP.runCreate
                      user2
                      (SPParams.CreateParam iBTCTokenName 3_125_000)
                      SP.OpenSucceed

                    SP.runCreate
                      user3
                      (SPParams.CreateParam iBTCTokenName 3_750_000)
                      SP.OpenSucceed

                    SP.runWithdraw
                      user3
                      (SPParams.WithdrawParam iBTCTokenName 100_000)

                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 10_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user2) FreezeSucceed
                    runLiquidate user2 initialLiquidateParam LiquidationSucceed

                    SP.runDeposit
                      user3
                      (SPParams.DepositParam iBTCTokenName 100_000)
                ),
              checkFailWithMsg'
                "During liquidation a different iAsset can't be burnt"
                (ErrorMsgContains "Expected burn all cdp auth tokens in cdp input and all minted iAssets")
                ( do
                    runInitialize initialOracles 1
                    user <- openPos' initialOpenPosParam Succeed
                    runOpenCDPPosition
                      user
                      initialOpenPosParam {opAsset = iETHTokenName}
                      Succeed
                    feedPrice
                      iBTCTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    feedPrice
                      iETHTokenName
                      (OnChainDecimal 1_000_000_000)
                      FeedPriceSucceed
                    runFreeze (initialFreezeParam user) FreezeSucceed
                    SP.runCreate
                      user
                      (SPParams.CreateParam iBTCTokenName 3_000_000)
                      SP.OpenSucceed
                    runLiquidate
                      user
                      initialLiquidateParam
                      (LiquidationMoreBurns (iETHTokenName, 1_000_000))
                )
            ],
          testGroup
            "EpochToScaleToSum specific"
            [ check
                "SP Close between 2 EpochToScaleToSum snapshots\
                \ (1, 1, 6) items"
                (spCloseCase ((1, 1), (1, 1), (5, 6))),
              check
                "SP Close between 2 EpochToScaleToSum snapshots\
                \ (5, 5, 6) items"
                (spCloseCase ((5, 5), (5, 5), (5, 6))),
              check
                "SP Deposit between 2 EpochToScaleToSum snapshots\
                \ (1, 1, 6) items"
                (spDepositCase ((1, 1), (1, 1), (5, 6))),
              check
                "SP Deposit between 2 EpochToScaleToSum snapshots\
                \ (5, 5, 6) items"
                (spDepositCase ((5, 5), (5, 5), (5, 6))),
              check
                "StabilityPool record epochToScaleToSum 5 items"
                (spRecordEpochToScaleToSumCase (5, 6)),
              checkFailWithMsg'
                "Liq. fails when EpochToScaleToSum has more than 5 items"
                ( ErrorMsgContains
                    "EpochToScaleToSum map is too large,\
                    \ use RecordEpochToScaleToSum"
                )
                ( do
                    runInitialize initialOracles 1
                    replicateM_
                      6
                      ( do
                          void $
                            createSPAccount 2_000_000_000_000 1_000_000_000_000
                          liquidateCase1 999_999_999_990
                      )
                ),
              checkFailWithMsg'
                "Steal account token when recording map fails"
                (ErrorMsgContains "Must consume Stability Pool correctly")
                stealWhenRecord
            ],
          check
            "Stability Pool's snapshotP does not get to 0 after multiple big liquidations"
            ( do
                otherUser <- newUser mempty
                runInitialize [(iBTCTokenName, OnChainDecimal 1_000_000, Active)] 1

                replicateM_
                  4
                  ( do
                      replicateM_
                        5
                        ( do
                            void $ createSPAccount 2_000_000_000_000 1_000_000_000_000
                            liquidateCase1 999_999_999_990
                        )
                      SP.runRecordEpochToScaleToSum
                        otherUser
                        (SPParams.RecordEpochToScaleToSumParam iBTCTokenName)
                        SP.RecordSucceed
                  )

                (_, _, snapshot, _) <- findStabilityPool iBTCTokenName
                -- The increase in epoch happened instead of making snapshotP 0
                unless
                  ( snapshotEpoch snapshot P.== 1
                  )
                  (logError "SP epoch was not increased to 1")
            )
        ]
      benchmarks =
        [ assertLimits
            initialFunds
            cfg
            "init ref script creator"
            runInitRefScriptCreator,
          assertLimits initialFunds cfg "init ref script" runInitRefScript,
          assertLimits
            initialFunds
            cfg
            "init ref script collector"
            runInitRefScriptCollector,
          assertLimits
            initialFunds
            cfg
            "open position"
            ( runInitialize initialOracles 1
                >> void (openPos' initialOpenPosParam Succeed)
            ),
          assertLimits
            initialFunds
            cfg
            "deposit"
            ( runInitialize initialOracles 1
                >> openPos' initialOpenPosParam Succeed
                >>= deposit' initialDepositParam AdjustSucceed
            ),
          assertLimits
            initialFunds
            cfg
            "withdraw"
            ( runInitialize initialOracles 1
                >> openPos' initialOpenPosParam Succeed
                >>= withdraw' initialWithdrawParam
            ),
          assertLimits
            initialFunds
            cfg
            "mint"
            ( runInitialize initialOracles 1
                >> openPos' initialOpenPosParam Succeed
                >>= mint' initialMintParam
            ),
          assertLimits
            initialFunds
            cfg
            "burn"
            ( runInitialize initialOracles 1
                >> openPos' initialOpenPosParam Succeed
                >>= burn' initialBurnParam
            ),
          assertLimits
            initialFunds
            cfg
            "close"
            ( runInitialize initialOracles 1
                >> openPos' initialOpenPosParam Succeed
                >>= close' initialCloseParam CloseSucceed
            ),
          assertLimits
            initialFunds
            cfg
            "freeze"
            ( do
                runInitialize initialOracles 1
                user <- openPos' initialOpenPosParam Succeed
                feedPrice
                  iBTCTokenName
                  (OnChainDecimal 1_000_000_000)
                  FeedPriceSucceed
                runFreeze (initialFreezeParam user) FreezeSucceed
            ),
          assertLimits
            initialFunds
            cfg
            "liquidate"
            ( do
                runInitialize initialOracles 1
                user <- openPos' initialOpenPosParam Succeed
                feedPrice
                  iBTCTokenName
                  (OnChainDecimal 1_000_000_000)
                  FeedPriceSucceed
                runFreeze (initialFreezeParam user) FreezeSucceed
                SP.runCreate
                  user
                  (SPParams.CreateParam iBTCTokenName 3_000_000)
                  SP.OpenSucceed
                runLiquidate user initialLiquidateParam LiquidationSucceed
            ),
          assertLimits
            initialFunds
            cfg
            "partial liquidate"
            ( do
                runInitialize initialOracles 1
                user <- openPos' initialOpenPosParam Succeed
                feedPrice
                  iBTCTokenName
                  (OnChainDecimal 1_000_000_000)
                  FeedPriceSucceed
                runFreeze (initialFreezeParam user) FreezeSucceed
                SP.runCreate
                  user
                  (SPParams.CreateParam iBTCTokenName 2_000_000)
                  SP.OpenSucceed
                runLiquidate user initialLiquidateParam LiquidationSucceed
            ),
          assertLimits initialFunds cfg "merge 2 CDPs" (mergeCDPsUseCase 2),
          assertLimits initialFunds cfg "merge 3 CDPs" (mergeCDPsUseCase 3),
          assertLimits initialFunds cfg "merge 4 CDPs" (mergeCDPsUseCase 4),
          assertLimits initialFunds cfg "merge 8 CDPs" (mergeCDPsUseCase 8),
          testGroup
            "EpochToScaleToSum specific"
            [ assertLimits
                initialFunds
                cfg
                "SP Close between 2 EpochToScaleToSum snapshots\
                \ (1, 1, 6) items"
                (spCloseCase ((1, 1), (1, 1), (5, 6))),
              assertLimits
                initialFunds
                cfg
                "SP Close between 2 EpochToScaleToSum snapshots\
                \ (5, 5, 6) items"
                (spCloseCase ((5, 5), (5, 5), (5, 6))),
              assertLimits
                initialFunds
                cfg
                "SP Deposit between 2 EpochToScaleToSum snapshots\
                \ (1, 1, 6) items"
                (spDepositCase ((1, 1), (1, 1), (5, 6))),
              assertLimits
                initialFunds
                cfg
                "SP Deposit between 2 EpochToScaleToSum snapshots\
                \ (5, 5, 6) items"
                (spDepositCase ((5, 5), (5, 5), (5, 6))),
              assertLimits
                initialFunds
                cfg
                "StabilityPool record epochToScaleToSum 5 items"
                (spRecordEpochToScaleToSumCase (5, 6))
            ]
        ]
   in testGroup "CDP" (setupTestBenchmarkSuite suiteOpts specs benchmarks)
  where
    check :: String -> Run () -> TestTree
    check = checkNoFail cfg initialFunds

    checkFailWithMsg' :: String -> ErrorMsgContains -> Run () -> TestTree
    checkFailWithMsg' = checkFailWithMsg cfg initialFunds

    deposit' ::
      (PubKeyHash -> DepositParam) -> AdjustVariation -> PubKeyHash -> Run ()
    deposit' depositParam v user = runDeposit (depositParam user) v

    withdraw' :: (PubKeyHash -> WithdrawParam) -> PubKeyHash -> Run ()
    withdraw' withdrawParam user = runWithdraw (withdrawParam user)

    mint' :: (PubKeyHash -> MintParam) -> PubKeyHash -> Run ()
    mint' mintParam user = runMint (mintParam user)

    burn' :: (PubKeyHash -> BurnParam) -> PubKeyHash -> Run ()
    burn' burnParam user = runBurn (burnParam user)

    close' ::
      (PubKeyHash -> CloseParam) -> CloseVariation -> PubKeyHash -> Run ()
    close' closeParam v user = runClose 0 (closeParam user) v

    closeWithContention' :: Run ()
    closeWithContention' = do
      runInitialize
        [ (iBTCTokenName, OnChainDecimal 1_000_000, Active),
          (iETHTokenName, OnChainDecimal 1_000_000, Active)
        ]
        2
      user1 <- newUser mempty
      user2 <- newUser mempty
      let opParams token =
            OpenPositionParam
              { opAsset = token,
                opCollateralAmount = 10_000_000,
                opMintedAmount = 3_000_000
              }
      runOpenCDPPosition user1 (opParams iBTCTokenName) Succeed
      runOpenCDPPosition user2 (opParams iETHTokenName) Succeed
      close1 <-
        createCloseTx
          0
          CloseParam
            { cCdpOwnerPkh = PaymentPubKeyHash user1,
              cAsset = iBTCTokenName
            }
          CloseSucceed
          >>= signTx user1
      close2 <-
        createCloseTx
          1
          CloseParam
            { cCdpOwnerPkh = PaymentPubKeyHash user2,
              cAsset = iETHTokenName
            }
          CloseSucceed
          >>= signTx user2
      void $ sendBlock [close1, close2]

    mergeCDPsUseCase :: Int -> Run ()
    mergeCDPsUseCase cdpCount = do
      admin <- getMainUser
      runInitialize initialOracles 1
      let openPosParam =
            OpenPositionParam
              { opAsset = iBTCTokenName,
                opCollateralAmount = 10_000_000,
                opMintedAmount = 3_000_000
              }
      users <- replicateM cdpCount (openPos' openPosParam Succeed)

      feedPrice iBTCTokenName (OnChainDecimal 1_000_000_000) FeedPriceSucceed
      mapM_
        ( \user ->
            runFreeze
              (FreezeParam {fCdpOwnerPkh = user, fAsset = iBTCTokenName})
              FreezeSucceed
        )
        users
      mergeAll admin iBTCTokenName

    mergeAll :: PubKeyHash -> Value.TokenName -> Run ()
    mergeAll user asset = do
      allFrozenCDPs <- findUtxos (cdpScript Mock.cdpParams) frozenCDPPredicate
      runMergeCDPs
        user
        ( MergeCDPsParam
            { cdpsToMerge = view _1 <$> allFrozenCDPs,
              mpAsset = asset
            }
        )
      where
        frozenCDPPredicate :: TxBox CDPScript -> Bool
        frozenCDPPredicate
          TxBox
            { txBoxDatum = CDP {cdpOwner = Nothing, cdpIAsset}
            } =
            cdpIAsset P.== asset
        frozenCDPPredicate _ = False

    sendFakeIAssetToCDP :: Value.TokenName -> Value -> Run ()
    sendFakeIAssetToCDP iAssetName val = do
      (_, _, iAsset) <- findIAsset iAssetName
      sendToScript (cdpScript Mock.cdpParams) (IAssetDatum iAsset) val

    sendToScript ::
      (FromData d, FromData r, ToData d, ToData r) =>
      TypedValidator d r ->
      d ->
      Value ->
      Run ()
    sendToScript script datum val = do
      admin <- getMainUser
      withSpend
        admin
        val
        ( \sp -> do
            let tx =
                  P.mconcat
                    [ userSpend sp,
                      payToRef script (InlineDatum datum) val
                    ]
            void $ signTx admin tx >>= sendTx
        )

initialFunds :: Value
initialFunds =
  Ada.adaValueOf 100_000_000
    <> Value.assetClassValue
      (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
      10_000_000
    <> Value.assetClassValue (CDPParams.iAssetAuthToken Mock.cdpParams) 2
    <> Value.assetClassValue (CDPParams.stabilityPoolAuthToken Mock.cdpParams) 2
    <> Value.assetClassValue (CDPParams.cdpCreatorNft Mock.cdpCreatorParams) 2
    <> GovernanceBenchmark.initialFunds
    <> OracleBenchmark.initialFunds

initialOracles :: [(V2.TokenName, OnChainDecimal, IAssetType)]
initialOracles =
  [ (iBTCTokenName, OnChainDecimal 1_000_000, Active),
    (iETHTokenName, OnChainDecimal 1_000_000, Active)
  ]

openPos' :: OpenPositionParam -> OpenPositionVariation -> Run PubKeyHash
openPos' opParams variation = do
  user <- newUser mempty
  runOpenCDPPosition user opParams variation
  P.pure user

createSPAccount :: Integer -> Integer -> Run Ledger.PubKeyHash
createSPAccount collateral mint = do
  userDepositor <-
    openPos'
      OpenPositionParam
        { opAsset = iBTCTokenName,
          opCollateralAmount = collateral,
          opMintedAmount = mint
        }
      Succeed
  SP.runCreate
    userDepositor
    (SPParams.CreateParam iBTCTokenName mint)
    SP.OpenSucceed
  P.pure userDepositor

depositToSP :: Integer -> Ledger.PubKeyHash -> Run ()
depositToSP amount user = do
  admin <- getMainUser
  sendValue
    admin
    ( Value.assetClassValue
        (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
        amount
    )
    user
  SP.runDeposit user (SPParams.DepositParam iBTCTokenName amount)

-- min collateral ratio has to be 150%
liquidateCase1 :: Integer -> Run ()
liquidateCase1 mint = do
  feedPrice iBTCTokenName (OnChainDecimal 1_000_000) FeedPriceSucceed
  user1 <-
    openPos'
      OpenPositionParam
        { opAsset = iBTCTokenName,
          opCollateralAmount = mint `P.div` 2 * 3,
          opMintedAmount = mint
        }
      Succeed
  feedPrice iBTCTokenName (OnChainDecimal 1_100_000) FeedPriceSucceed
  runFreeze
    (FreezeParam {fCdpOwnerPkh = user1, fAsset = iBTCTokenName})
    FreezeSucceed
  runLiquidate user1 LiquidateParam {lAsset = iBTCTokenName} LiquidationSucceed
  feedPrice iBTCTokenName (OnChainDecimal 1_000_000) FeedPriceSucceed

type ExpectedEssItems = Int

-- | Stability pool account creation
-- (combination of EpochToScaleToSum snapshots).
-- The `lastAction` happens between 2 epochToScaleToSum snapshots.
-- (5, 5, 5) number of items in the map for snapshot1,
-- snapshot2 and current SP epochToScaleToSum.
spAccountCreationCase ::
  ( (Int, ExpectedEssItems),
    (Int, ExpectedEssItems),
    (Int, ExpectedEssItems)
  ) ->
  (Ledger.PubKeyHash -> Run ()) ->
  Run ()
spAccountCreationCase
  ( (liquidationsCount1, snap1ItemsCount),
    (liquidationsCount2, snap2ItemsCount),
    (liquidationsCount3, currentEssItemsCount)
    )
  lastAction = do
    otherUser <- newUser mempty
    runInitialize [(iBTCTokenName, OnChainDecimal 1_000_000, Active)] 1

    replicateM_
      (liquidationsCount1 P.- 1)
      ( do
          void $ createSPAccount 2_000_000_000_000 1_000_000_000_000
          liquidateCase1 999_999_999_990
      )
    userDepositor <- createSPAccount 2_000_000_000_000 1_000_000_000_000
    -- One more liquidation that will remain in SP datum
    liquidateCase1 999_999_999_990

    findStabilityPool iBTCTokenName >>= \(_, _, _, ess) ->
      let itemsCount = P.length $ AssocMap.toList ess
       in unless
            -- `itemsCount - 1` because 1 item has to remain in the pool's datum EpochToScaleToSum map
            ( (itemsCount P.- 1) P.== snap1ItemsCount
            )
            (logError $ "Wrong EpochToScaleToSum items count (case 1). Items count = " P.<> P.show itemsCount)

    SP.runRecordEpochToScaleToSum
      otherUser
      (SPParams.RecordEpochToScaleToSumParam iBTCTokenName)
      SP.RecordSucceed

    replicateM_
      liquidationsCount2
      ( do
          void $ createSPAccount 2_000_000_000_000 1_000_000_000_000
          liquidateCase1 999_999_999_990
      )

    findStabilityPool iBTCTokenName >>= \(_, _, _, ess) ->
      let itemsCount = P.length $ AssocMap.toList ess
       in unless
            -- `itemsCount - 1` because 1 item has to remain in the pool's datum EpochToScaleToSum map
            ( (itemsCount P.- 1) P.== snap2ItemsCount
            )
            (logError $ "Wrong EpochToScaleToSum items count (case 2). Items count = " P.<> P.show itemsCount)

    SP.runRecordEpochToScaleToSum
      otherUser
      (SPParams.RecordEpochToScaleToSumParam iBTCTokenName)
      SP.RecordSucceed
    replicateM_
      liquidationsCount3
      ( do
          void $ createSPAccount 2_000_000_000_000 1_000_000_000_000
          liquidateCase1 999_999_999_990
      )

    findStabilityPool iBTCTokenName >>= \(_, _, _, ess) ->
      let itemsCount = P.length $ AssocMap.toList ess
       in unless
            ( itemsCount P.== currentEssItemsCount
            )
            (logError $ "Wrong EpochToScaleToSum items count (case 3). Items count = " P.<> P.show itemsCount)

    lastAction userDepositor

spCloseCase ::
  ( (Int, ExpectedEssItems),
    (Int, ExpectedEssItems),
    (Int, ExpectedEssItems)
  ) ->
  Run ()
spCloseCase param = do
  spAccountCreationCase
    param
    ( \depositor ->
        SP.runClose
          depositor
          SPParams.CloseParam {SPParams.clTokenName = iBTCTokenName}
    )

spDepositCase ::
  ( (Int, ExpectedEssItems),
    (Int, ExpectedEssItems),
    (Int, ExpectedEssItems)
  ) ->
  Run ()
spDepositCase param =
  spAccountCreationCase param (depositToSP 1_000_000)

spRecordEpochToScaleToSumCase :: (Int, ExpectedEssItems) -> Run ()
spRecordEpochToScaleToSumCase (liquidationsCount, expectedEssItemsCount) = do
  otherUser <- newUser mempty
  runInitialize initialOracles 1

  replicateM_
    liquidationsCount
    ( do
        void $ createSPAccount 2_000_000_000_000 1_000_000_000_000
        liquidateCase1 999_999_999_990
    )

  findStabilityPool iBTCTokenName >>= \(_, _, _, ess) ->
    let itemsCount = P.length $ AssocMap.toList ess
     in unless
          ( expectedEssItemsCount P.== itemsCount
          )
          (logError ("Wrong EpochToScaleToSum items count. Items count = " <> P.show itemsCount))

  SP.runRecordEpochToScaleToSum
    otherUser
    (SPParams.RecordEpochToScaleToSumParam iBTCTokenName)
    SP.RecordSucceed

  (_, _, snapshot, _) <- findStabilityPool iBTCTokenName
  unless
    (snapshotScale snapshot P.== P.fromIntegral (expectedEssItemsCount P.- 1))
    (logError "Wrong scale")

stealWhenRecord :: Run ()
stealWhenRecord = do
  otherUser <- newUser mempty
  runInitialize initialOracles 1
  depositor <- createSPAccount 2_000_000_000_000 1_000_000_000_000
  liquidateCase1 999_999_999_990
  SP.runRecordEpochToScaleToSum
    otherUser
    (SPParams.RecordEpochToScaleToSumParam iBTCTokenName)
    (SP.StealAccountToken depositor iBTCTokenName)
