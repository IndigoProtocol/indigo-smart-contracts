module Spec.Staking.Benchmark
  ( tests,
    initialFunds,
  )
where

import Control.Monad (replicateM_, void)
import Indigo.Contracts.Governance.Gov.Common qualified as Gov
import Indigo.Contracts.Governance.Poll.Common qualified as Poll
import Indigo.Contracts.Staking.Common qualified as StakingParams
import Indigo.Utils.Helpers (unitValue)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Value (Value, assetClassValue)
import Options (SuiteOptions, setupTestBenchmarkSuite)
import Plutus.Model
  ( DatumMode (InlineDatum),
    MockConfig,
    Run,
    checkBalance,
    getMainUser,
    gives,
    newUser,
    owns,
    payToScriptUntyped,
    sendTx,
    signTx,
    spend,
    userSpend,
    utxoAt,
    waitNSlots,
  )
import PlutusTx.Prelude hiding (check, mconcat)
import Spec.Collector.Transactions
  ( runCreateNewCollectorUtxo,
    runInitCollector,
    runInitCollectorWithStakeCredential,
    runSendFeeToCollector,
  )
import Spec.Governance.Params
  ( CreateProposalParam (CreateProposalParam),
    CreateShardsParam (CreateShardsParam),
    VoteParam (VoteParam),
  )
import Spec.Governance.Transactions
  ( runCreateProposal,
    runCreateShards,
    runInitGov,
    runVote,
  )
import Spec.Staking.Asserts (assertLengthVoteMap)
import Spec.Staking.Params
  ( OpenStakingPositionParam (OpenStakingPositionParam, oAmount),
    StakeParam (StakeParam, sAmount),
  )
import Spec.Staking.Script (alwaysPassingValidator, stakingScript)
import Spec.Staking.Transactions
  ( DistributeVariation
      ( DistributeLess,
        DistributeNoCollectorInput,
        DistributeRedeemerOtherScript,
        DistributeSucceed,
        DistributeWithStakingKey,
        MintStakingToken
      ),
    OpenPositionVariation
      ( OpAdditionalStakingPositionMint,
        OpDoubleSatisfaction,
        OpIncorrectPositionSnapshot,
        OpIncorrectTotalStake,
        OpSignedByOtherUser,
        OpWithDistribute,
        OpenSucceed
      ),
    StakeVariation (StakeSucceed, StakeWithDistribute),
    UnlockVariation (UnlockSucceed, UnlockWithDistribute),
    UnstakeVariation
      ( UnstakeExtraRewards,
        UnstakeSucceed,
        UnstakeWithDistribute
      ),
    runDistribute,
    runDoubleSatisfactionUnlock,
    runInitRefScript,
    runInitStaking,
    runOpenStakingPos,
    runStake,
    runUnlock,
    runUnstake,
  )
import Test.Tasty (TestTree, testGroup)
import Utils.Helpers
  ( ErrorMsgContains (ErrorMsgContains),
    assertLimits,
    checkFailWithMsg,
    checkNoFail,
    minLovelacesPerUtxo,
  )
import Utils.Mock qualified as Mock
import Prelude qualified as P

tests :: SuiteOptions -> MockConfig -> TestTree
tests suiteOpts cfg =
  let specs =
        [ testGroup
            "Initialization"
            [ check "Successfully initialized Staking Contract" runInitStaking
            ],
          testGroup
            "Open Staking Position"
            [ check
                "Successfully opens staking position."
                (void $ openStakingPos' OpenSucceed),
              checkFailWithMsg'
                "Opening staking position with another account fails."
                ( ErrorMsgContains
                    "Transaction must be signed by owner of staking position"
                )
                (void $ openStakingPos' OpSignedByOtherUser),
              checkFailWithMsg'
                "Minting two staking position tokens fails."
                (ErrorMsgContains "Must mint exactly 1 stakingToken")
                (void $ openStakingPos' OpAdditionalStakingPositionMint),
              checkFailWithMsg'
                "Setting incorrect StakingManager.totalStake fails."
                (ErrorMsgContains "Incorrect StakingManager output")
                (void $ openStakingPos' OpIncorrectTotalStake),
              checkFailWithMsg'
                "Setting incorrect StakingPosition.stakeAmount fails."
                (ErrorMsgContains "Incorrect StakingPosition output")
                (void $ openStakingPos' OpIncorrectPositionSnapshot),
              checkFailWithMsg'
                "Double satisfaction attack opening staking position fails"
                (ErrorMsgContains "Can not mint any token")
                doubleSatOpenStakingPos
            ],
          testGroup
            "Adjust Staking Position"
            [ check
                "Deposit additional stake succeeds"
                (depositStake' StakeSucceed),
              check "Withdraw stake succeeds" withdrawStake',
              check
                "Depositing stake after vote succeeds"
                (void stakeAfterVote'),
              checkFailWithMsg'
                "Withdrawing stake after vote fails"
                (ErrorMsgContains "Exceed maximum withdrawal amount")
                withdrawStakeAfterVote',
              check
                "After voting, deposit then withdraw stake succeeds"
                depositAndWithdrawStakeAfterVote'
            ],
          testGroup
            "Unlock INDY"
            [ check
                "Successfully unlocks all INDY"
                ( do
                    user <- unlockAll UnlockSucceed
                    assertLengthVoteMap user 0
                ),
              check
                "Successfully unlocks part of locked INDY"
                ( do
                    user <- unlockPart UnlockSucceed
                    assertLengthVoteMap user 1
                ),
              check
                "Successfully keeps original amount locked"
                ( do
                    user <- unlockNone UnlockSucceed
                    assertLengthVoteMap user 2
                ),
              checkFailWithMsg'
                "Double satisfaction attack fails"
                ( ErrorMsgContains
                    "Expected exactly one input with single token"
                )
                doubleSatisfactionUnlock
            ],
          testGroup
            "Unstake Staking Position"
            [ check
                "Successfully unstakes staking position."
                (unstake' UnstakeSucceed),
              check "Successfully unstakes after voting" voteUnlockAndUnstake,
              checkFailWithMsg'
                "Unstake without unlocking fails"
                (ErrorMsgContains "Can not unstake locked fund")
                voteAndUnstake,
              checkFailWithMsg'
                "Unstake claiming extra rewards fails"
                (ErrorMsgContains "Incorrect StakingManager output")
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 750_000_000)
                    user1 <- newUser mempty
                    user2 <- newUser mempty
                    -- 1000 INDY
                    runOpenStakingPos
                      user1
                      (OpenStakingPositionParam {oAmount = 1_000_000_000})
                      OpenSucceed
                    -- 2000 INDY
                    runOpenStakingPos
                      user2
                      (OpenStakingPositionParam {oAmount = 2_000_000_000})
                      OpenSucceed
                    runDistribute DistributeSucceed
                    runUnstake user1 UnstakeExtraRewards
                )
            ],
          testGroup
            "Distribute collected fees"
            [ check
                "distributing more than 95% of the collector's value succeeds"
                -- This is distributing the min possible amount, less cannot be
                -- distributed because of the minLovelacesPerUtxo (2 ADA).
                -- This is because 95% of 40 ADA is 2 ADA.
                ( do
                    runInitStaking
                    runInitCollector 1 (Just 38_000_020)
                    user <- newUser mempty
                    runOpenStakingPos
                      user
                      (OpenStakingPositionParam {oAmount = 100})
                      OpenSucceed
                    runDistribute DistributeSucceed
                ),
              checkFailWithMsg'
                "distributing less than 95% of the collector inputs' value fails"
                ( ErrorMsgContains
                    "More than 95% of the distributable value \
                    \has to be distributed"
                )
                -- Total value of collector inputs here is 40_000_020 Lovelaces.
                -- To be successful, more than 38_000_019 Lovelaces need to be
                -- distributed.
                ( do
                    runInitStaking
                    runInitCollector 1 (Just 38_000_020)
                    user <- newUser mempty
                    runOpenStakingPos
                      user
                      (OpenStakingPositionParam {oAmount = 100})
                      OpenSucceed
                    runDistribute (DistributeLess minLovelacesPerUtxo)
                ),
              check
                "Distribution with multiple collector inputs succeeds"
                (distributionWithMultipleUtxos 2),
              checkFailWithMsg'
                "Distribution fails when no collector inputs"
                ( ErrorMsgContains
                    "More than 95% of the distributable value \
                    \has to be distributed"
                )
                ( do
                    runInitStaking
                    runInitCollector 1 (Just minLovelacesPerUtxo)
                    user <- newUser mempty
                    runOpenStakingPos
                      user
                      (OpenStakingPositionParam {oAmount = 100})
                      OpenSucceed
                    runDistribute DistributeNoCollectorInput
                ),
              checkFailWithMsg'
                "distribution fails when not more than 95% of the collector \
                \inputs' value is reached"
                ( ErrorMsgContains
                    "More than 95% of the distributable value \
                    \has to be distributed"
                )
                -- If more than 95% of the collector's value were distributed
                -- then the collector's UTXO would have less than the min ADA.
                -- So it's not possible.
                ( do
                    runInitStaking
                    runInitCollector 1 (Just 38_000_019)
                    user <- newUser mempty
                    runOpenStakingPos
                      user
                      (OpenStakingPositionParam {oAmount = 100})
                      OpenSucceed
                    runDistribute DistributeSucceed
                ),
              checkFailWithMsg'
                "distribute while attaching staking key to collector fails"
                ( ErrorMsgContains
                    "Fail to return the same number of UTxOs\
                    \ to the collector address"
                )
                ( do
                    runInitStaking
                    runInitCollector 1 (Just (minLovelacesPerUtxo + 2))
                    user <- newUser mempty
                    runOpenStakingPos
                      user
                      (OpenStakingPositionParam {oAmount = 100})
                      OpenSucceed
                    runDistribute (DistributeWithStakingKey Nothing (Just user))
                ),
              checkFailWithMsg'
                "distribute a collector UTxO with attached staking credential\
                \ and keep the staking credential fails"
                ( ErrorMsgContains
                    "Fail to return the same number of UTxOs\
                    \ to the collector address"
                )
                ( do
                    runInitStaking
                    user <- newUser mempty
                    runInitCollectorWithStakeCredential
                      user
                      1
                      (Just 38_000_020)
                    runOpenStakingPos
                      user
                      (OpenStakingPositionParam {oAmount = 100})
                      OpenSucceed
                    runDistribute
                      (DistributeWithStakingKey (Just user) (Just user))
                ),
              check
                "distribute a collector UTxO with attached staking credential\
                \ and remove the staking credential succeeds"
                ( do
                    runInitStaking
                    user <- newUser mempty
                    -- There exists a collector UTxO with a staking credential
                    runInitCollectorWithStakeCredential
                      user
                      1
                      (Just 38_000_020)
                    runOpenStakingPos
                      user
                      (OpenStakingPositionParam {oAmount = 100})
                      OpenSucceed
                    -- The staking credential gets removed when distributing
                    runDistribute (DistributeWithStakingKey (Just user) Nothing)
                    -- ADA is added to the collector UTxO with no staking cred.
                    runSendFeeToCollector 0 38_000_020
                    -- The resulting UTxO can be distributed normally
                    runDistribute DistributeSucceed
                ),
              checkFailWithMsg'
                "distribute fails trying to mint"
                (ErrorMsgContains "Can not mint any token")
                ( do
                    runInitStaking
                    runInitCollector 1 (Just minLovelacesPerUtxo)
                    user <- newUser mempty
                    runOpenStakingPos
                      user
                      (OpenStakingPositionParam {oAmount = 100})
                      OpenSucceed
                    runDistribute MintStakingToken
                ),
              checkFailWithMsg'
                "distribute fails when trying to open position"
                (ErrorMsgContains "Must use Distribute redeemer")
                ( do
                    runInitCollector 1 (Just 20_000_000)
                    void $
                      openStakingPos'
                        (OpWithDistribute (20_000_000 + minLovelacesPerUtxo))
                ),
              checkFailWithMsg'
                "distribute fails when trying to unstake"
                (ErrorMsgContains "Must use Distribute redeemer")
                ( do
                    runInitCollector 1 (Just 20_000_000)
                    unstake'
                      (UnstakeWithDistribute (20_000_000 + minLovelacesPerUtxo))
                ),
              checkFailWithMsg'
                "distribute fails when trying to unlock"
                (ErrorMsgContains "Must use Distribute redeemer")
                ( do
                    runInitCollector 1 (Just 20_000_000)
                    void $
                      unlockAll
                        (UnlockWithDistribute (20_000_000 + minLovelacesPerUtxo))
                ),
              checkFailWithMsg'
                "distribute fails when trying to stake"
                (ErrorMsgContains "Must use Distribute redeemer")
                ( do
                    runInitCollector 1 (Just 20_000_000)
                    depositStake'
                      (StakeWithDistribute (20_000_000 + minLovelacesPerUtxo))
                ),
              checkFailWithMsg'
                "distribute fails when used with other script"
                ( ErrorMsgContains
                    "Must use Distribute redeemer or increase ADA value \
                    \with just 1 input"
                )
                ( do
                    admin <- getMainUser
                    -- If the `spendVal` additionally had stakingManagerNFT,
                    -- then the funds could be stolen.
                    -- But since it can't be minted and leave the protocol,
                    -- this attack can't be executed.
                    let spendVal = Ada.lovelaceValueOf minLovelacesPerUtxo
                    sp <- spend admin spendVal
                    let tx =
                          P.mconcat
                            [ userSpend sp,
                              payToScriptUntyped
                                alwaysPassingValidator
                                (InlineDatum ())
                                spendVal
                            ]
                    void $ signTx admin tx >>= sendTx

                    [(oref, o)] <- utxoAt alwaysPassingValidator

                    runInitStaking
                    runInitCollector 1 (Just minLovelacesPerUtxo)
                    user <- newUser mempty
                    runOpenStakingPos
                      user
                      (OpenStakingPositionParam {oAmount = 100})
                      OpenSucceed
                    runDistribute (DistributeRedeemerOtherScript o oref)
                )
            ],
          testGroup
            "Protocol Fees"
            [ checkFailWithMsg'
                "Cannot distribute with no INDY staked"
                (ErrorMsgContains "No INDY stakers")
                cannotDistributeWithNoStakers,
              checkFailWithMsg'
                "Cannot distribute with no INDY staked, after staking"
                (ErrorMsgContains "No INDY stakers")
                cannotDistributeWithNoStakersAfterStaking,
              check
                "Can reward INDY stakers from Unstake action"
                canRewardStakersFromUnstake,
              check
                "Can reward INDY stakers from Stake action"
                canRewardStakersFromStake,
              check
                "Final balances after distribution and unstaking are correct"
                distributeUnstakeCheckBalances,
              checkFailWithMsg'
                "Test 1"
                (ErrorMsgContains "No INDY stakers")
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 750_000_000)
                    runDistribute DistributeSucceed
                ),
              check
                "Test 2"
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 750_000_000)
                    user1 <- newUser mempty
                    -- 1000 INDY
                    runOpenStakingPos
                      user1
                      (OpenStakingPositionParam {oAmount = 1_000_000_000})
                      OpenSucceed
                    runDistribute DistributeSucceed
                    checkBalance
                      ( owns
                          user1
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 750_000_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                1_000_000_000
                          )
                      )
                      (runUnstake user1 UnstakeSucceed)
                ),
              check
                "Test 3"
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 750_000_000)
                    user1 <- newUser mempty
                    user2 <- newUser mempty
                    -- 1000 INDY
                    runOpenStakingPos
                      user1
                      (OpenStakingPositionParam {oAmount = 1_000_000_000})
                      OpenSucceed
                    runDistribute DistributeSucceed
                    -- 2000 INDY
                    runOpenStakingPos
                      user2
                      (OpenStakingPositionParam {oAmount = 2_000_000_000})
                      OpenSucceed
                    checkBalance
                      ( owns
                          user1
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 750_000_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                1_000_000_000
                          )
                      )
                      (runUnstake user1 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user2
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                2_000_000_000
                          )
                      )
                      (runUnstake user2 UnstakeSucceed)
                ),
              check
                "Test 4"
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 750_000_000)
                    user1 <- newUser mempty
                    user2 <- newUser mempty
                    -- 1000 INDY
                    runOpenStakingPos
                      user1
                      (OpenStakingPositionParam {oAmount = 1_000_000_000})
                      OpenSucceed
                    -- 2000 INDY
                    runOpenStakingPos
                      user2
                      (OpenStakingPositionParam {oAmount = 2_000_000_000})
                      OpenSucceed
                    runDistribute DistributeSucceed
                    checkBalance
                      ( owns
                          user1
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 250_000_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                1_000_000_000
                          )
                      )
                      (runUnstake user1 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user2
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 500_000_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                2_000_000_000
                          )
                      )
                      (runUnstake user2 UnstakeSucceed)
                ),
              check
                "Test 5"
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 1000_000_000)
                    user1 <- newUser mempty
                    user2 <- newUser mempty
                    user3 <- newUser mempty
                    -- 1000 INDY
                    runOpenStakingPos
                      user1
                      (OpenStakingPositionParam {oAmount = 1_000_000_000})
                      OpenSucceed
                    -- 2000 INDY
                    runOpenStakingPos
                      user2
                      (OpenStakingPositionParam {oAmount = 2_000_000_000})
                      OpenSucceed
                    -- 3000 INDY
                    runOpenStakingPos
                      user3
                      (OpenStakingPositionParam {oAmount = 3_000_000_000})
                      OpenSucceed
                    runDistribute DistributeSucceed
                    checkBalance
                      ( owns
                          user1
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 166_666_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                1_000_000_000
                          )
                      )
                      (runUnstake user1 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user2
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 333_332_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                2_000_000_000
                          )
                      )
                      (runUnstake user2 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user3
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 499_998_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                3_000_000_000
                          )
                      )
                      (runUnstake user3 UnstakeSucceed)
                ),
              check
                "Test 6"
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 1593_000_000)
                    user1 <- newUser mempty
                    user2 <- newUser mempty
                    user3 <- newUser mempty
                    -- 1000 INDY
                    runOpenStakingPos
                      user1
                      (OpenStakingPositionParam {oAmount = 1_000_000_000})
                      OpenSucceed
                    -- 2000 INDY
                    runOpenStakingPos
                      user2
                      (OpenStakingPositionParam {oAmount = 2_000_000_000})
                      OpenSucceed
                    -- 3000 INDY
                    runOpenStakingPos
                      user3
                      (OpenStakingPositionParam {oAmount = 3_000_000_000})
                      OpenSucceed
                    runDistribute DistributeSucceed
                    checkBalance
                      ( owns
                          user1
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 265_500_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                1_000_000_000
                          )
                      )
                      (runUnstake user1 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user2
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 531_000_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                2_000_000_000
                          )
                      )
                      (runUnstake user2 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user3
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 796_500_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                3_000_000_000
                          )
                      )
                      (runUnstake user3 UnstakeSucceed)
                ),
              check
                "Test 7"
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 9_999_000_000)
                    user1 <- newUser mempty
                    user2 <- newUser mempty
                    user3 <- newUser mempty
                    -- 1234 INDY
                    runOpenStakingPos
                      user1
                      (OpenStakingPositionParam {oAmount = 1_234_000_000})
                      OpenSucceed
                    -- 2341 INDY
                    runOpenStakingPos
                      user2
                      (OpenStakingPositionParam {oAmount = 2_341_000_000})
                      OpenSucceed
                    -- 3333 INDY
                    runOpenStakingPos
                      user3
                      (OpenStakingPositionParam {oAmount = 3_333_000_000})
                      OpenSucceed
                    runDistribute DistributeSucceed
                    checkBalance
                      ( owns
                          user1
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 1_786_155_768
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                1_234_000_000
                          )
                      )
                      (runUnstake user1 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user2
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 3_388_485_132
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                2_341_000_000
                          )
                      )
                      (runUnstake user2 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user3
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 4_824_357_516
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                3_333_000_000
                          )
                      )
                      (runUnstake user3 UnstakeSucceed)
                ),
              check
                "Test 8"
                -- Scaled by a factor of 2 wrt the original test to be above
                -- the minimum distributable value.
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 47_750_168)
                    user1 <- newUser mempty
                    user2 <- newUser mempty
                    user3 <- newUser mempty
                    user4 <- newUser mempty
                    -- 28565.101948 INDY
                    runOpenStakingPos
                      user1
                      (OpenStakingPositionParam {oAmount = 28_565_101_948})
                      OpenSucceed
                    -- 1616.489394 INDY
                    runOpenStakingPos
                      user2
                      (OpenStakingPositionParam {oAmount = 1_616_489_394})
                      OpenSucceed
                    -- 9917.540954 INDY
                    runOpenStakingPos
                      user3
                      (OpenStakingPositionParam {oAmount = 9_917_540_954})
                      OpenSucceed
                    -- 165.914682 INDY
                    runOpenStakingPos
                      user4
                      (OpenStakingPositionParam {oAmount = 165_914_682})
                      OpenSucceed
                    runDistribute DistributeSucceed
                    checkBalance
                      ( owns
                          user1
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              -- Doubling the reward allows an extra lovelace
                              <> Ada.lovelaceValueOf 33_849_645
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                28_565_101_948
                          )
                      )
                      (runUnstake user1 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user2
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              -- Doubling the reward allows an extra lovelace
                              <> Ada.lovelaceValueOf 1_915_539
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                1_616_489_394
                          )
                      )
                      (runUnstake user2 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user3
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 11_752_286
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                9_917_540_954
                          )
                      )
                      (runUnstake user3 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user4
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 196_608
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                165_914_682
                          )
                      )
                      (runUnstake user4 UnstakeSucceed)
                ),
              check
                "Test 9"
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 750_000_000)
                    user1 <- newUser mempty
                    user2 <- newUser mempty
                    -- 1000 INDY
                    runOpenStakingPos
                      user1
                      (OpenStakingPositionParam {oAmount = 1_000_000_000})
                      OpenSucceed
                    -- 2000 INDY
                    runOpenStakingPos
                      user2
                      (OpenStakingPositionParam {oAmount = 2_000_000_000})
                      OpenSucceed
                    checkBalance
                      ( owns
                          user2
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                2_000_000_000
                          )
                      )
                      (runUnstake user2 UnstakeSucceed)

                    runDistribute DistributeSucceed

                    checkBalance
                      ( owns
                          user1
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 750_000_000
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                1_000_000_000
                          )
                      )
                      (runUnstake user1 UnstakeSucceed)
                ),
              check
                "Test 10"
                -- Scaled by a factor of 2 wrt the original test to be above
                -- the minimum distributable value.
                ( do
                    runInitGov
                    runInitStaking
                    runInitCollector 1 (Just 47_750_168)
                    user1 <- newUser mempty
                    user2 <- newUser mempty
                    user3 <- newUser mempty
                    user4 <- newUser mempty
                    -- 28565.101948 INDY
                    runOpenStakingPos
                      user1
                      (OpenStakingPositionParam {oAmount = 28_565_101_948})
                      OpenSucceed
                    -- 1616.489394 INDY
                    runOpenStakingPos
                      user2
                      (OpenStakingPositionParam {oAmount = 1_616_489_394})
                      OpenSucceed
                    -- 9917.540954 INDY
                    runOpenStakingPos
                      user3
                      (OpenStakingPositionParam {oAmount = 9_917_540_954})
                      OpenSucceed
                    -- 165.914682 INDY
                    runOpenStakingPos
                      user4
                      (OpenStakingPositionParam {oAmount = 165_914_682})
                      OpenSucceed

                    runDistribute DistributeSucceed

                    checkBalance
                      ( owns
                          user3
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 11_752_286
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                9_917_540_954
                          )
                      )
                      (runUnstake user3 UnstakeSucceed)

                    runSendFeeToCollector 0 15_680_003_154
                    runDistribute DistributeSucceed

                    checkBalance
                      ( owns
                          user1
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              -- Doubling the reward allows an extra lovelace
                              <> Ada.lovelaceValueOf 14_792_895_085
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                28_565_101_948
                          )
                      )
                      (runUnstake user1 UnstakeSucceed)
                    checkBalance
                      ( owns
                          user2
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 837_124_896
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                1_616_489_394
                          )
                      )
                      (runUnstake user2 UnstakeSucceed)

                    checkBalance
                      ( owns
                          user4
                          ( Ada.lovelaceValueOf minLovelacesPerUtxo
                              <> Ada.lovelaceValueOf 85_921_572
                              <> assetClassValue
                                (StakingParams.indyToken Mock.stakeParams)
                                165_914_682
                          )
                      )
                      (runUnstake user4 UnstakeSucceed)
                )
            ]
        ]
      benchmarks =
        [ assertLimits initialFunds cfg "init ref script" runInitRefScript,
          assertLimits
            initialFunds
            cfg
            "open position"
            (void $ openStakingPos' OpenSucceed),
          assertLimits initialFunds cfg "unstake" (unstake' UnstakeSucceed),
          assertLimits initialFunds cfg "stake" (depositStake' StakeSucceed),
          assertLimits
            initialFunds
            cfg
            "distribute (2 UTXOs)"
            (distributionWithMultipleUtxos 2),
          assertLimits
            initialFunds
            cfg
            "distribute (3 UTXOs)"
            (distributionWithMultipleUtxos 3),
          assertLimits
            initialFunds
            cfg
            "distribute (4 UTXOs)"
            (distributionWithMultipleUtxos 4),
          assertLimits
            initialFunds
            cfg
            "distribute (5 UTXOs)"
            (distributionWithMultipleUtxos 5),
          assertLimits
            initialFunds
            cfg
            "distribute (7 UTXOs)"
            (distributionWithMultipleUtxos 7),
          assertLimits
            initialFunds
            cfg
            "distribute (9 UTXOs)"
            (distributionWithMultipleUtxos 9),
          assertLimits
            initialFunds
            cfg
            "distribute (11 UTXOs)"
            (distributionWithMultipleUtxos 11)
        ]
   in testGroup "Staking" (setupTestBenchmarkSuite suiteOpts specs benchmarks)
  where
    check :: P.String -> Run () -> TestTree
    check = checkNoFail cfg initialFunds

    checkFailWithMsg' :: P.String -> ErrorMsgContains -> Run () -> TestTree
    checkFailWithMsg' = checkFailWithMsg cfg initialFunds

    openStakingPos' :: OpenPositionVariation -> Run Ledger.PubKeyHash
    openStakingPos' v = do
      runInitStaking
      let openStakingParams = OpenStakingPositionParam {oAmount = 100}
      user <- newUser mempty
      runOpenStakingPos user openStakingParams v
      return user

    doubleSatOpenStakingPos :: Run ()
    doubleSatOpenStakingPos = do
      runInitStaking
      let openStakingParams = OpenStakingPositionParam {oAmount = 100}
      admin <- getMainUser
      void $ runOpenStakingPos admin openStakingParams OpenSucceed
      void $ runOpenStakingPos admin openStakingParams OpDoubleSatisfaction

    unstake' :: UnstakeVariation -> Run ()
    unstake' variation = do
      runInitStaking
      let openStakingParams = OpenStakingPositionParam {oAmount = 100}
      admin <- getMainUser
      runOpenStakingPos admin openStakingParams OpenSucceed
      runUnstake admin variation

    depositStake' :: StakeVariation -> Run ()
    depositStake' variation = do
      user <- openStakingPos' OpenSucceed
      runStake user (StakeParam {sAmount = 100_000}) variation

    withdrawStake' :: Run ()
    withdrawStake' = do
      user <- openStakingPos' OpenSucceed
      runStake user (StakeParam {sAmount = -50}) StakeSucceed

    runVote' :: Run Ledger.PubKeyHash
    runVote' = do
      runInitGov
      runInitStaking
      let openStakingParams = OpenStakingPositionParam {oAmount = 100}
      user <- newUser mempty
      runOpenStakingPos user openStakingParams OpenSucceed
      runStake user (StakeParam {sAmount = 50_000}) StakeSucceed
      (pId1, eTime1, tShards1) <-
        runCreateProposal $
          CreateProposalParam
            (Gov.TextProposal (P.mconcat $ P.replicate 64 "a"))
      runCreateShards (CreateShardsParam pId1 eTime1 tShards1)
      runVote user (VoteParam pId1 Poll.Yes)
      (pId2, eTime2, tShards2) <-
        runCreateProposal $
          CreateProposalParam
            (Gov.TextProposal (P.mconcat $ P.replicate 64 "a"))
      runCreateShards (CreateShardsParam pId2 eTime2 tShards2)
      runVote user (VoteParam pId2 Poll.Yes)
      return user

    stakeAfterVote' :: Run Ledger.PubKeyHash
    stakeAfterVote' = do
      user <- runVote'
      runStake user (StakeParam {sAmount = 50_000}) StakeSucceed
      return user

    withdrawStakeAfterVote' :: Run ()
    withdrawStakeAfterVote' = do
      user <- runVote'
      runStake user (StakeParam {sAmount = -50_000}) StakeSucceed

    depositAndWithdrawStakeAfterVote' :: Run ()
    depositAndWithdrawStakeAfterVote' = do
      user <- stakeAfterVote'
      runStake user (StakeParam {sAmount = -50_000}) StakeSucceed

    unlockAll :: UnlockVariation -> Run Ledger.PubKeyHash
    unlockAll variation = do
      user <- runVote'
      waitNSlots 20
      runUnlock user variation
      return user

    unlockPart :: UnlockVariation -> Run Ledger.PubKeyHash
    unlockPart variation = do
      user <- runVote'
      waitNSlots 5
      runUnlock user variation
      return user

    unlockNone :: UnlockVariation -> Run Ledger.PubKeyHash
    unlockNone variation = do
      user <- runVote'
      runUnlock user variation
      return user

    voteUnlockAndUnstake :: Run ()
    voteUnlockAndUnstake = do
      user <- unlockAll UnlockSucceed
      runUnstake user UnstakeSucceed

    voteAndUnstake :: Run ()
    voteAndUnstake = do
      user <- runVote'
      runUnstake user UnstakeSucceed

    doubleSatisfactionUnlock :: Run ()
    doubleSatisfactionUnlock = do
      runInitGov
      runInitStaking
      let openStakingParams = OpenStakingPositionParam {oAmount = 100}
      user <- newUser mempty
      runOpenStakingPos user openStakingParams OpenSucceed
      runOpenStakingPos user openStakingParams OpenSucceed
      runDoubleSatisfactionUnlock user

    distributionWithMultipleUtxos :: P.Int -> Run ()
    distributionWithMultipleUtxos rewardUtxos = do
      runInitStaking
      runInitCollector 1 (Just 38_000_020)
      user <- newUser mempty
      runOpenStakingPos
        user
        (OpenStakingPositionParam {oAmount = 100})
        OpenSucceed
      replicateM_
        (rewardUtxos P.- 1)
        (runCreateNewCollectorUtxo 38_000_000)
      runDistribute DistributeSucceed

    cannotDistributeWithNoStakers :: Run ()
    cannotDistributeWithNoStakers = do
      runInitGov
      runInitStaking
      runInitCollector 1 (Just 38_000_020)
      runDistribute DistributeSucceed

    cannotDistributeWithNoStakersAfterStaking :: Run ()
    cannotDistributeWithNoStakersAfterStaking = do
      runInitGov
      runInitStaking
      runInitCollector 1 (Just 38_000_020)
      user <- newUser mempty
      runOpenStakingPos
        user
        (OpenStakingPositionParam {oAmount = 100})
        OpenSucceed
      runUnstake user UnstakeSucceed
      runDistribute DistributeSucceed

    canRewardStakersFromUnstake :: Run ()
    canRewardStakersFromUnstake = do
      runInitGov
      runInitStaking
      runInitCollector 1 (Just 38_000_020)
      user <- newUser mempty
      runOpenStakingPos
        user
        (OpenStakingPositionParam {oAmount = 100})
        OpenSucceed
      runDistribute DistributeSucceed
      runUnstake user UnstakeSucceed

    canRewardStakersFromStake :: Run ()
    canRewardStakersFromStake = do
      runInitGov
      runInitStaking
      runInitCollector 1 (Just 38_000_020)
      user <- newUser mempty
      runOpenStakingPos
        user
        (OpenStakingPositionParam {oAmount = 100})
        OpenSucceed
      runDistribute DistributeSucceed
      runStake user (StakeParam {sAmount = -50}) StakeSucceed

    distributeUnstakeCheckBalances :: Run ()
    distributeUnstakeCheckBalances = do
      runInitGov
      runInitStaking
      runInitCollector 1 (Just 750_000_000)
      user <- newUser mempty
      runOpenStakingPos
        user
        (OpenStakingPositionParam {oAmount = 1000})
        OpenSucceed
      runDistribute DistributeSucceed
      void $
        checkBalance
          ( P.mconcat
              [ -- Staker is ditributed 750 ADA,
                -- also recovering 0.001_000 INDY and 2 ADA
                gives
                  (stakingScript Mock.stakeParams)
                  ( Ada.lovelaceValueOf minLovelacesPerUtxo
                      <> Ada.lovelaceValueOf 750_000_000
                      <> assetClassValue
                        (StakingParams.indyToken Mock.stakeParams)
                        1000
                  )
                  user,
                -- 1 staking_token is burnt
                owns
                  (stakingScript Mock.stakeParams)
                  ( inv
                      ( assetClassValue
                          (StakingParams.stakingToken Mock.stakeParams)
                          1
                      )
                  )
              ]
          )
          (runUnstake user UnstakeSucceed)

initialFunds :: Value
initialFunds =
  Ada.adaValueOf 10_000_000
    <> unitValue (Gov.govNFT Mock.govParams)
    <> unitValue (StakingParams.stakingManagerNFT Mock.stakeParams)
    <> assetClassValue
      (StakingParams.indyToken Mock.stakeParams)
      100_000_000_000
