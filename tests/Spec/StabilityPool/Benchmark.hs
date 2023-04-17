module Spec.StabilityPool.Benchmark
  ( tests,
  )
where

import Indigo.Contracts.CDP.Common
  ( CDPScriptParams (cdpAssetSymbol),
    iBTCTokenName,
    protocolFee,
  )
import Indigo.Contracts.Governance.Gov.Common qualified as GovParams
import Indigo.Contracts.StabilityPool.Common hiding (StabilityPoolScript)
import Indigo.Data.Decimal (getOnChainInt)
import Indigo.Utils.Helpers (unitValue)
import Ledger (Value)
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
import Options (SuiteOptions, setupTestBenchmarkSuite)
import Plutus.Model
import PlutusTx.Prelude hiding (check)
import Spec.StabilityPool.Params
  ( CloseParam (CloseParam, clTokenName),
    CreateParam (CreateParam, cAmount, cTokenName),
    DepositParam (DepositParam, dAmount, dTokenName),
    WithdrawParam (WithdrawParam, wAmount, wTokenName),
  )
import Spec.StabilityPool.Transactions
  ( OpenVariation (ModifyPoolStakingCredential, OpenSucceed),
    runClose,
    runCreate,
    runDeposit,
    runInitRefScript,
    runInitStabilityPool,
    runInitialize,
    runSpendAccountWithSP,
    runWithdraw,
  )
import Test.Tasty
import Utils.Helpers
  ( ErrorMsgContains (ErrorMsgContains),
    assertLimits,
    checkFailWithMsg,
    checkFails,
    checkNoFail,
    minLovelacesPerUtxo,
  )
import Utils.Mock qualified as Mock
import Prelude (Integral (div), String)

tests :: SuiteOptions -> MockConfig -> TestTree
tests suiteOpts cfg =
  let specs =
        [ check "create succeeds" create',
          checkFailWithMsg'
            "user cannot withdraw more than deposited"
            (ErrorMsgContains "User cannot have negative balance")
            ( do
                runInitialize 1
                runInitStabilityPool iBTCTokenName
                user1 <-
                  newUser
                    ( Value.assetClassValue
                        (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
                        1
                    )
                user2 <-
                  newUser
                    ( Value.assetClassValue
                        (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
                        2
                    )
                runCreate
                  user1
                  (CreateParam {cAmount = 1, cTokenName = iBTCTokenName})
                  OpenSucceed
                runCreate
                  user2
                  (CreateParam {cAmount = 2, cTokenName = iBTCTokenName})
                  OpenSucceed

                runWithdraw user1 WithdrawParam {wTokenName = iBTCTokenName, wAmount = 2}
            ),
          check "adjust succeeds" adjust',
          check "adjust withdraw succeeds" adjustWithdraw',
          check "close succeeds" close',
          check
            "close succeeds with SP fee rewards check case 1"
            ( do
                runInitialize 1
                runInitStabilityPool iBTCTokenName
                admin <- getMainUser
                runCreate
                  admin
                  (CreateParam {cAmount = 1_000_000, cTokenName = iBTCTokenName})
                  OpenSucceed
                -- Gets all his deposit back (since no liquidations happened) +
                -- his fee for account creation (since he's the only SP user)
                checkBalance
                  ( let adaReward = accountCreateFeeLovelaces Mock.spParams
                        fee =
                          protocolFee
                            (GovParams.protocolFeePercentage Mock.protocolParams)
                            adaReward
                            0
                     in owns
                          admin
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf (adaReward - fee)
                              <> Value.assetClassValue
                                (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
                                1_000_000
                          )
                  )
                  ( runClose admin CloseParam {clTokenName = iBTCTokenName}
                  )
            ),
          check
            "close succeeds with SP fee rewards check case 2"
            ( do
                runInitialize 1
                runInitStabilityPool iBTCTokenName
                admin <- getMainUser
                user1 <-
                  newUser $
                    Value.assetClassValue
                      (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
                      1_000_000
                runCreate
                  admin
                  (CreateParam {cAmount = 1_000_000, cTokenName = iBTCTokenName})
                  OpenSucceed
                runCreate
                  user1
                  (CreateParam {cAmount = 1_000_000, cTokenName = iBTCTokenName})
                  OpenSucceed
                -- Gets all his iAsset deposit back (since no liquidations happened) +
                -- his fee for account creation (since he's the only SP user) +
                -- half of user1 account creation fee
                checkBalance
                  ( let adaReward = accountCreateFeeLovelaces Mock.spParams `div` 2 * 3
                        fee =
                          protocolFee
                            (GovParams.protocolFeePercentage Mock.protocolParams)
                            adaReward
                            0
                     in owns
                          admin
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf (adaReward - fee)
                              <> Value.assetClassValue
                                (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
                                1_000_000
                          )
                  )
                  ( runClose admin CloseParam {clTokenName = iBTCTokenName}
                  )
                -- Gets all his iAsset deposit back (since no liquidations happened) +
                -- half of his account creation fee
                checkBalance
                  ( let adaReward = accountCreateFeeLovelaces Mock.spParams `div` 2
                        fee =
                          protocolFee
                            (GovParams.protocolFeePercentage Mock.protocolParams)
                            adaReward
                            0
                     in owns
                          user1
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf (adaReward - fee)
                              <> Value.assetClassValue
                                (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
                                1_000_000
                          )
                  )
                  ( runClose user1 CloseParam {clTokenName = iBTCTokenName}
                  )
            ),
          checkFailWithMsg'
            "changing stability pool staking credential fails"
            (ErrorMsgContains "Stability Pool output does not match")
            createAndModifyStakingCredential,
          checkFails'
            "withdraw all funds with SpendAccount redeemer\
            \ against StabilityPool output fails"
            spendAccountWithSP'
        ]
      benchmarks =
        [ assertLimits initialFunds cfg "init ref script" runInitRefScript,
          assertLimits initialFunds cfg "create" create',
          assertLimits initialFunds cfg "adjust" adjust',
          assertLimits initialFunds cfg "adjust (withdraw)" adjustWithdraw',
          assertLimits initialFunds cfg "close" close'
        ]
   in testGroup
        "Stability pool"
        (setupTestBenchmarkSuite suiteOpts specs benchmarks)
  where
    check :: String -> Run () -> TestTree
    check = checkNoFail cfg initialFunds

    checkFails' :: String -> Run () -> TestTree
    checkFails' = checkFails cfg initialFunds

    checkFailWithMsg' :: String -> ErrorMsgContains -> Run () -> TestTree
    checkFailWithMsg' = checkFailWithMsg cfg initialFunds

    create' :: Run ()
    create' = do
      admin <- getMainUser
      runInitStabilityPool iBTCTokenName
      runCreate
        admin
        (CreateParam {cAmount = 1_000_000_000_000, cTokenName = iBTCTokenName})
        OpenSucceed

    createAndModifyStakingCredential :: Run ()
    createAndModifyStakingCredential = do
      admin <- getMainUser
      runInitStabilityPool iBTCTokenName
      runCreate
        admin
        (CreateParam {cAmount = 1_000_000_000_000, cTokenName = iBTCTokenName})
        ModifyPoolStakingCredential

    adjust' :: Run ()
    adjust' = do
      runInitialize 1
      admin <- getMainUser
      runInitStabilityPool iBTCTokenName
      let amount :: Integer
          amount = 1_000_000_000_000
      runCreate
        admin
        CreateParam {cAmount = amount, cTokenName = iBTCTokenName}
        OpenSucceed
      runDeposit
        admin
        DepositParam {dTokenName = iBTCTokenName, dAmount = amount}

    adjustWithdraw' :: Run ()
    adjustWithdraw' = do
      runInitialize 1
      admin <- getMainUser
      runInitStabilityPool iBTCTokenName
      let amount :: Integer
          amount = 1_000_000_000_000
          amountWithdraw = 500
      runCreate
        admin
        CreateParam
          { cAmount = amount,
            cTokenName = iBTCTokenName
          }
        OpenSucceed
      runWithdraw
        admin
        WithdrawParam {wTokenName = iBTCTokenName, wAmount = amountWithdraw}

    close' :: Run ()
    close' = do
      runInitialize 1
      admin <- getMainUser
      runInitStabilityPool iBTCTokenName
      runCreate
        admin
        (CreateParam {cAmount = 1_000_000_000_000, cTokenName = iBTCTokenName})
        OpenSucceed
      runClose admin CloseParam {clTokenName = iBTCTokenName}

    spendAccountWithSP' :: Run ()
    spendAccountWithSP' = do
      create'
      runSpendAccountWithSP iBTCTokenName

initialFunds :: Value
initialFunds =
  Ada.adaValueOf 10_000_000
    <> unitValue (stabilityPoolToken Mock.spParams)
    <> Value.assetClassValue
      (Value.assetClass (cdpAssetSymbol Mock.cdpParams) iBTCTokenName)
      (getOnChainInt 2_000_000)
    <> unitValue (GovParams.govNFT Mock.govParams)
