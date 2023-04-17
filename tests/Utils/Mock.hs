{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Utils.Mock
  ( iaRatio,
    govTk,
    spParams,
    cdpParams,
    cdpCreatorParams,
    collectorParams,
    stakeParams,
    executeParams,
    pollParams,
    pollManagerParams,
    govParams,
    vrParams,
    oracleParams,
    treasuryParams,
    initialIndyDistribution,
    totalINDYSupply,
    distributionSchedule,
    protocolParams,
  )
where

import Indigo.Contracts.CDP.Common qualified as CDPParams
import Indigo.Contracts.Collector.Common qualified as CollectorParams
import Indigo.Contracts.Governance.Execute.Common qualified as ExecuteParams
import Indigo.Contracts.Governance.Gov.Common qualified as GovParams
import Indigo.Contracts.Governance.Poll.Common (DistributionSchedule (..))
import Indigo.Contracts.Governance.Poll.Common qualified as PollParams
import Indigo.Contracts.Governance.VersionRegistry.Common qualified as VRParams
import Indigo.Contracts.Oracle.Common
import Indigo.Contracts.StabilityPool.Common qualified as SPParams
import Indigo.Contracts.Staking.Common qualified as StakingParams
import Indigo.Contracts.Treasury.Common qualified as TreasuryParams
import Indigo.Data.Decimal
import Indigo.Data.Token (fakeCurrencySymbol)
import Indigo.Utils.Helpers
import Indigo.Utils.Spooky qualified as Spooky
import Ledger.Value
import Plutus.Model.V2 (Run, getMainUser, validatorHash)
import PlutusTx.Prelude hiding (pure, (*))
import Spec.CDP.Script (cdpScript, iAssetSymbol)
import Spec.Collector.Script (collectorScript)
import Spec.Governance.Script
  ( executeScript,
    pollManagerScript,
    pollScript,
    versionRecordSymbol,
    versionRegistryScript,
  )
import Spec.StabilityPool.Script (stabilityPoolScript)
import Spec.Staking.Script (stakingScript)
import Spec.Treasury.Script (treasuryScript)
import Utils.MintPolicies (authPolicySymbol)
import Prelude (pure, (*))

iaRatio :: OnChainDecimal
iaRatio = 150

minCollateralInLovelace :: Integer
minCollateralInLovelace = 10_000_000 -- 10 ADA

authTokenAssetClass :: AssetClass -> TokenName -> AssetClass
authTokenAssetClass nft tn = assetClass (authPolicySymbol nft tn) tn

-- | The simulated asset class of the token identifies
-- authentic Stability pool outputs
spToken :: AssetClass
spToken = authTokenAssetClass upgradeTk SPParams.stabilityPoolTokenName

-- | The simulated asset class of the token identifies
-- authentic iAsset outputs
iaToken :: AssetClass
iaToken = authTokenAssetClass upgradeTk "iAsset_token"

-- | The asset class of the NFT identifies authentic CDPCreator outputs
cdpCreatorToken :: AssetClass
cdpCreatorToken = assetClass fakeCurrencySymbol CDPParams.cdpCreatorTokenName

-- | The asset class of the token identifies an authentic CDP outputs
cdpToken :: AssetClass
cdpToken = authTokenAssetClass cdpCreatorToken CDPParams.cdpTokenName

-- | The simulated asset class of the token identifies an authentic stake pool
stakeNft :: AssetClass
stakeNft = assetClass fakeCurrencySymbol "staking_manager_nft"

-- | The asset class of the token identifies an authentic poll outputs
pollTk :: AssetClass
pollTk = authTokenAssetClass govTk "poll_token"

-- | The asset class of the token identifies an authentic
-- epoch to scale to sum map output
snapshotEpochToScaleToSumTk :: AssetClass
snapshotEpochToScaleToSumTk =
  authTokenAssetClass spToken SPParams.snapshotEpochToScaleToSumTokenName

-- | The simulated asset class of the token identifies an authentic
-- upgrade output
upgradeTk :: AssetClass
upgradeTk = authTokenAssetClass pollTk "upgrade_token"

-- | The asset class of the token identifies an authentic stake output
stakeTk :: AssetClass
stakeTk = authTokenAssetClass stakeNft "staking_token"

-- | The simulated asset class of the token identifies the authentic gov output
govTk :: AssetClass
govTk = assetClass fakeCurrencySymbol "govTk"

-- | The simulated asset class of the INDY token
indyAssetClass :: AssetClass
indyAssetClass = assetClass fakeCurrencySymbol "indyAsset"

-- | The asset class of the token identifies an authentic version record token
versionRecordToken :: AssetClass
versionRecordToken =
  assetClass (versionRecordSymbol vrParams) ExecuteParams.versionRecordTokenName

-- | The asset class of the token identifies authentic Account outputs
accToken :: AssetClass
accToken = authTokenAssetClass spToken SPParams.accountTokenName

-- | The default parameters for the stability pool script
spParams :: SPParams.StabilityPoolParams
spParams =
  SPParams.StabilityPoolParams
    { SPParams.assetSymbol = iAssetSymbol cdpToken,
      SPParams.stabilityPoolToken = spToken,
      SPParams.snapshotEpochToScaleToSumToken = snapshotEpochToScaleToSumTk,
      SPParams.accountToken = accToken,
      SPParams.cdpToken = cdpToken,
      SPParams.versionRecordToken,
      SPParams.collectorValHash =
        validatorHash (collectorScript collectorParams),
      SPParams.govNFT = govTk,
      SPParams.accountCreateFeeLovelaces = 10 * decimalUnit,
      SPParams.accountAdjustmentFeeLovelaces = 2 * decimalUnit
    }

cdpParams :: CDPParams.CDPScriptParams
cdpParams =
  CDPParams.CDPScriptParams
    { CDPParams.cdpAuthToken = cdpToken,
      CDPParams.cdpAssetSymbol = iAssetSymbol cdpToken,
      CDPParams.iAssetAuthToken = iaToken,
      CDPParams.versionRecordToken,
      CDPParams.stabilityPoolAuthToken = spToken,
      CDPParams.upgradeToken = upgradeTk,
      CDPParams.collectorValHash =
        validatorHash (collectorScript collectorParams),
      CDPParams.spValHash = validatorHash (stabilityPoolScript spParams),
      CDPParams.govNFT = govTk,
      CDPParams.minCollateralInLovelace = minCollateralInLovelace
    }

cdpCreatorParams :: CDPParams.CDPCreatorScriptParams
cdpCreatorParams =
  CDPParams.CDPCreatorScriptParams
    { CDPParams.cdpCreatorNft = cdpCreatorToken,
      CDPParams.cdpAssetCs = CDPParams.cdpAssetSymbol cdpParams,
      CDPParams.cdpAuthTk = CDPParams.cdpAuthToken cdpParams,
      CDPParams.iAssetAuthTk = CDPParams.iAssetAuthToken cdpParams,
      CDPParams.cdpScriptHash = validatorHash (cdpScript cdpParams),
      CDPParams.versionRecordToken,
      CDPParams.minCollateralInLovelace = minCollateralInLovelace
    }

-- | The default parameters for the collector script
collectorParams :: CollectorParams.CollectorScriptParams
collectorParams =
  CollectorParams.CollectorScriptParams
    { CollectorParams.stakingManagerNFT = Spooky.toSpookyAssetClass stakeNft,
      CollectorParams.stakingToken = Spooky.toSpookyAssetClass stakeTk,
      CollectorParams.versionRecordToken =
        Spooky.toSpookyAssetClass versionRecordToken
    }

treasuryParams :: TreasuryParams.TreasuryScriptParams
treasuryParams =
  TreasuryParams.MkTreasuryScriptParams
    { TreasuryParams.versionRecordToken
    }

stakeParams :: StakingParams.StakingParams
stakeParams =
  StakingParams.StakingParams
    { StakingParams.stakingManagerNFT = stakeNft,
      StakingParams.stakingToken = stakeTk,
      StakingParams.indyToken = indyAssetClass,
      StakingParams.pollToken = pollTk,
      StakingParams.collectorValHash =
        validatorHash (collectorScript collectorParams),
      StakingParams.versionRecordToken
    }

executeParams :: ExecuteParams.ExecuteParams
executeParams =
  ExecuteParams.ExecuteParams
    { ExecuteParams.govNFT = govTk,
      ExecuteParams.upgradeToken = upgradeTk,
      ExecuteParams.iAssetToken = iaToken,
      ExecuteParams.stabilityPoolToken = spToken,
      ExecuteParams.versionRecordToken,
      ExecuteParams.cdpValHash = validatorHash (cdpScript cdpParams),
      ExecuteParams.sPoolValHash = validatorHash (stabilityPoolScript spParams),
      ExecuteParams.versionRegistryValHash = validatorHash versionRegistryScript
    }

initialIndyDistribution :: Integer
initialIndyDistribution = 1_575_000_000_000

totalINDYSupply :: Integer
totalINDYSupply = 35_000_000_000_000

-- | Ref: https://drive.google.com/uc?id=12K0tzNAy0Ef1OQBRE6unofAH-szIVB1a
distributionSchedule :: DistributionSchedule
distributionSchedule =
  MkDistributionSchedule
    { λM_spd =
        [ OnChainDecimal 60_000,
          OnChainDecimal 70_000,
          OnChainDecimal 80_000,
          OnChainDecimal 90_000,
          OnChainDecimal 100_000
        ],
      z_spd = 6,
      λM_lpd =
        [ OnChainDecimal 10_000,
          OnChainDecimal 20_000,
          OnChainDecimal 30_000,
          OnChainDecimal 40_000,
          OnChainDecimal 50_000
        ],
      z_lpd = 18,
      λM_ipd =
        [ OnChainDecimal 5_000,
          OnChainDecimal 7_500,
          OnChainDecimal 10_000,
          OnChainDecimal 12_500,
          OnChainDecimal 15_000
        ],
      z_ipd = 0,
      λM_tv = OnChainDecimal 225_000,
      z_tv = 21
    }

pollParams :: PollParams.PollParams
pollParams =
  PollParams.PollParams
    { PollParams.pollToken = Spooky.toSpookyAssetClass pollTk,
      PollParams.stakingToken = Spooky.toSpookyAssetClass stakeTk,
      PollParams.indyAsset = Spooky.toSpookyAssetClass indyAssetClass,
      PollParams.stakingValHash =
        Spooky.toSpookyValidatorHash (validatorHash $ stakingScript stakeParams)
    }

pollManagerParams :: PollParams.PollManagerParams
pollManagerParams =
  PollParams.PollManagerParams
    { PollParams.govNFT = govTk,
      PollParams.pollToken = pollTk,
      PollParams.upgradeToken = upgradeTk,
      PollParams.stakingToken = stakeTk,
      PollParams.indyAsset = indyAssetClass,
      PollParams.govExecuteValHash =
        validatorHash (executeScript executeParams),
      PollParams.stakingValHash = validatorHash (stakingScript stakeParams),
      PollParams.pBiasTime = 20 * oneSecond,
      PollParams.treasuryValHash =
        validatorHash (treasuryScript treasuryParams),
      PollParams.initialIndyDistribution = initialIndyDistribution,
      PollParams.totalINDYSupply = totalINDYSupply,
      PollParams.distributionSchedule = distributionSchedule,
      PollParams.shardsValHash = validatorHash (pollScript pollParams)
    }

govParams :: GovParams.GovParams
govParams =
  GovParams.GovParams
    { GovParams.govNFT = govTk,
      GovParams.pollToken = pollTk,
      GovParams.upgradeToken = upgradeTk,
      GovParams.versionRecordToken,
      GovParams.indyAsset = indyAssetClass,
      GovParams.pollManagerValHash =
        validatorHash (pollManagerScript pollManagerParams),
      GovParams.gBiasTime = 20 * oneSecond
    }

protocolParams :: GovParams.ProtocolParams
protocolParams =
  GovParams.ProtocolParams
    { proposalDeposit = 1_000_000_000,
      votingPeriod = 10_000,
      effectiveDelay = 2_000,
      expirationPeriod = 20_000,
      protocolFeePercentage = OnChainDecimal 1_500_000,
      -- High value of proposingPeriod is required for creation of shards
      -- in chunks of 2.
      -- Sane value is at least lower than votingPeriod
      proposingPeriod = 5_000,
      totalShards = 10
    }

vrParams :: VRParams.VersionRecordParams
vrParams = VRParams.VersionRecordParams {VRParams.upgradeToken = upgradeTk}

-- | The admin is always the creator/owner of the oracle
oracleParams :: Run OracleParams
oracleParams = do
  admin <- getMainUser
  pure $
    OracleParams
      (Spooky.toSpookyPubKeyHash admin)
      (20 * oneSecond)
      (oneSecond * 300)
