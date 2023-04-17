module Main (main) where

import Data.ByteString.Base16
import Indigo.Contracts.CDP.Common (CDPCreatorScriptParams (..), CDPScriptParams (..))
import Indigo.Contracts.CDP.OnChain (untypedCDPCreatorValidatorHash, untypedCDPScriptHash, untypedIAssetPolicySymbol)
import Indigo.Contracts.Collector.Common (CollectorScriptParams (..))
import Indigo.Contracts.Collector.OnChain (untypedCollectorHash)
import Indigo.Contracts.Governance.Execute.Common (ExecuteParams (..))
import Indigo.Contracts.Governance.Execute.OnChain (untypedExecuteHash)
import Indigo.Contracts.Governance.Gov.Common (GovParams (..))
import Indigo.Contracts.Governance.Gov.OnChain (untypedGovHash)
import Indigo.Contracts.Governance.Poll.Common (DistributionSchedule (MkDistributionSchedule), PollManagerParams (..), PollParams (..), z_ipd, z_lpd, z_spd, z_tv, λM_ipd, λM_lpd, λM_spd, λM_tv)
import Indigo.Contracts.Governance.Poll.OnChain (untypedPollManagerValidatorHash, untypedPollValidatorHash)
import Indigo.Contracts.Governance.VersionRegistry.Common (VersionRecordParams (..))
import Indigo.Contracts.Governance.VersionRegistry.OnChain (untypedVersionRecordMintSymbol, untypedVersionRegistryValidatorHash)
import Indigo.Contracts.Liquidity.OnChain (untypedLiquidityValidatorHash)
import Indigo.Contracts.Oracle.Common (OracleParams (..))
import Indigo.Contracts.Oracle.OnChain (untypedOracleValidatorHash)
import Indigo.Contracts.StabilityPool.Common (StabilityPoolParams (..))
import Indigo.Contracts.StabilityPool.OnChain (untypedStabilityPoolValidatorHash)
import Indigo.Contracts.Staking.Common (StakingParams (..))
import Indigo.Contracts.Staking.OnChain (untypedStakingValidatorHash)
import Indigo.Contracts.Treasury.Common (TreasuryScriptParams (..))
import Indigo.Contracts.Treasury.OnChain (untypedTreasuryValidatorHash)
import Indigo.Data.Token (untypedAuthTokenSymbol)
import Indigo.Utils.Spooky qualified as Spooky
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import Prelude

main :: IO ()
main = do
  let Right csBS = decode "a912171ffbe2d7b06e464905fd79320c3a1bab8529900ec29caaed8e"
      fakeCS = Value.currencySymbol csBS
      fakeAssetClass = Value.assetClass fakeCS "FOO"
      fakeValHash = "a912171ffbe2d7b06e464905fd79320c3a1bab8529900ec29caaed8e"
      fakePubKeyHash = Ledger.PubKeyHash (V2.toBuiltin csBS)

  print $
    "Auth token policy: "
      <> show
        ( untypedAuthTokenSymbol (PlutusTx.toBuiltinData fakeAssetClass) (PlutusTx.toBuiltinData ("FOO" :: Value.TokenName))
        )

  print $
    "CDP: "
      <> show
        ( untypedCDPScriptHash
            ( PlutusTx.toBuiltinData
                CDPScriptParams
                  { cdpAuthToken = fakeAssetClass,
                    cdpAssetSymbol = fakeCS,
                    iAssetAuthToken = fakeAssetClass,
                    stabilityPoolAuthToken = fakeAssetClass,
                    versionRecordToken = fakeAssetClass,
                    upgradeToken = fakeAssetClass,
                    collectorValHash = fakeValHash,
                    spValHash = fakeValHash,
                    govNFT = fakeAssetClass,
                    minCollateralInLovelace = 1_000_000
                  }
            )
        )

  print $
    "CDP creator: "
      <> show
        ( untypedCDPCreatorValidatorHash
            ( PlutusTx.toBuiltinData
                CDPCreatorScriptParams
                  { cdpCreatorNft = fakeAssetClass,
                    cdpAssetCs = fakeCS,
                    cdpAuthTk = fakeAssetClass,
                    iAssetAuthTk = fakeAssetClass,
                    versionRecordToken = fakeAssetClass,
                    cdpScriptHash = fakeValHash,
                    minCollateralInLovelace = 1_000_000
                  }
            )
        )

  print $
    "iAsset mint: "
      <> show
        ( untypedIAssetPolicySymbol
            ( PlutusTx.toBuiltinData
                ( Spooky.toSpookyAssetClass fakeAssetClass
                )
            )
        )

  print $
    "Collector: "
      <> show
        ( untypedCollectorHash
            ( PlutusTx.toBuiltinData
                CollectorScriptParams
                  { stakingManagerNFT = Spooky.toSpookyAssetClass fakeAssetClass,
                    stakingToken = Spooky.toSpookyAssetClass fakeAssetClass,
                    versionRecordToken = Spooky.toSpookyAssetClass fakeAssetClass
                  }
            )
        )

  print $
    "Execute: "
      <> show
        ( untypedExecuteHash
            ( PlutusTx.toBuiltinData
                ExecuteParams
                  { govNFT = fakeAssetClass,
                    upgradeToken = fakeAssetClass,
                    iAssetToken = fakeAssetClass,
                    stabilityPoolToken = fakeAssetClass,
                    versionRecordToken = fakeAssetClass,
                    cdpValHash = fakeValHash,
                    sPoolValHash = fakeValHash,
                    versionRegistryValHash = fakeValHash
                  }
            )
        )

  print $
    "Gov: "
      <> show
        ( untypedGovHash
            ( PlutusTx.toBuiltinData
                GovParams
                  { govNFT = fakeAssetClass,
                    pollToken = fakeAssetClass,
                    upgradeToken = fakeAssetClass,
                    indyAsset = fakeAssetClass,
                    versionRecordToken = fakeAssetClass,
                    pollManagerValHash = fakeValHash,
                    gBiasTime = 0
                  }
            )
        )

  print $
    "Poll: "
      <> show
        ( untypedPollValidatorHash
            ( PlutusTx.toBuiltinData
                PollParams
                  { pollToken = Spooky.toSpookyAssetClass fakeAssetClass,
                    stakingToken = Spooky.toSpookyAssetClass fakeAssetClass,
                    indyAsset = Spooky.toSpookyAssetClass fakeAssetClass,
                    stakingValHash = Spooky.toSpookyValidatorHash fakeValHash
                  }
            )
        )

  print $
    "Poll manager: "
      <> show
        ( untypedPollManagerValidatorHash
            ( PlutusTx.toBuiltinData
                PollManagerParams
                  { govNFT = fakeAssetClass,
                    pollToken = fakeAssetClass,
                    upgradeToken = fakeAssetClass,
                    stakingToken = fakeAssetClass,
                    indyAsset = fakeAssetClass,
                    govExecuteValHash = fakeValHash,
                    stakingValHash = fakeValHash,
                    pBiasTime = 0,
                    shardsValHash = fakeValHash,
                    treasuryValHash = fakeValHash,
                    initialIndyDistribution = 0,
                    totalINDYSupply = 0,
                    distributionSchedule =
                      MkDistributionSchedule
                        { λM_spd = [],
                          z_spd = 0,
                          λM_lpd = [],
                          z_lpd = 0,
                          λM_ipd = [],
                          z_ipd = 0,
                          λM_tv = 0,
                          z_tv = 0
                        }
                  }
            )
        )

  print $
    "Version record policy: "
      <> show
        ( untypedVersionRecordMintSymbol
            ( PlutusTx.toBuiltinData
                VersionRecordParams
                  { upgradeToken = fakeAssetClass
                  }
            )
        )

  print $ "Version registry: " <> show untypedVersionRegistryValidatorHash

  print $ "Liquidity: " <> show untypedLiquidityValidatorHash

  print $
    "Oracle: "
      <> show
        ( untypedOracleValidatorHash
            ( PlutusTx.toBuiltinData
                OracleParams
                  { opOwner = Spooky.toSpookyPubKeyHash fakePubKeyHash,
                    opBiasTime = 0,
                    opExpirationTime = 0
                  }
            )
        )

  print $
    "Stability pool: "
      <> show
        ( untypedStabilityPoolValidatorHash
            ( PlutusTx.toBuiltinData
                StabilityPoolParams
                  { assetSymbol = fakeCS,
                    stabilityPoolToken = fakeAssetClass,
                    snapshotEpochToScaleToSumToken = fakeAssetClass,
                    accountToken = fakeAssetClass,
                    cdpToken = fakeAssetClass,
                    versionRecordToken = fakeAssetClass,
                    collectorValHash = fakeValHash,
                    govNFT = fakeAssetClass,
                    accountCreateFeeLovelaces = 1_000_000,
                    accountAdjustmentFeeLovelaces = 1_000_000
                  }
            )
        )

  print $
    "Staking: "
      <> show
        ( untypedStakingValidatorHash
            ( PlutusTx.toBuiltinData
                StakingParams
                  { stakingManagerNFT = fakeAssetClass,
                    stakingToken = fakeAssetClass,
                    indyToken = fakeAssetClass,
                    pollToken = fakeAssetClass,
                    versionRecordToken = fakeAssetClass,
                    collectorValHash = fakeValHash
                  }
            )
        )

  print $
    "Treasury: "
      <> show
        ( untypedTreasuryValidatorHash
            ( PlutusTx.toBuiltinData
                MkTreasuryScriptParams
                  { versionRecordToken = fakeAssetClass
                  }
            )
        )
