-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE TemplateHaskell #-}

module Indigo.Data.Token
  ( fakeValidator,
    fakeMintingPolicy,
    fakeMintingPolicyHash,
    fakeCurrencySymbol,
    fakeLPToken,
    indyTokenName,
    mkAuthTokenPolicy,
    authTokenPolicy,
    authTokenPolicyHash,
    authTokenSymbol,
    authTokenCTL,
    untypedAuthTokenSymbol,
    untypedAuthTokenPolicyHash,
  )
where

import Indigo.Utils.Helpers qualified as Helpers
import Indigo.Utils.Spooky qualified as Spooky
import Indigo.Utils.Spooky.Helpers qualified as Spooky.Helpers
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies
  ( UntypedMintingPolicy,
    mkUntypedMintingPolicy,
  )
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Prelude

{-
Policy/Currency Symbol/TokenName for fake INDY token.
Use for testing purpose.
-}
{-# INLINEABLE fakeValidator #-}
fakeValidator :: () -> V2.ScriptContext -> Bool
fakeValidator _ _ = True

fakeMintingPolicy :: V2.MintingPolicy
fakeMintingPolicy =
  V2.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkUntypedMintingPolicy fakeValidator||])

fakeMintingPolicyHash :: V2.MintingPolicyHash
fakeMintingPolicyHash = PV2.mintingPolicyHash fakeMintingPolicy

fakeCurrencySymbol :: Value.CurrencySymbol
fakeCurrencySymbol = Value.mpsSymbol fakeMintingPolicyHash

fakeLPToken :: Value.AssetClass
fakeLPToken = Value.assetClass fakeCurrencySymbol "LPToken 1"

indyTokenName :: Value.TokenName
indyTokenName = "INDY"

{-
Throughout many contracts and scripts, there is a pattern of Minting Policy that
occurs repeatedly:
~
We need to consume a single "central"/"global"/"aggregation" UTxO to mint the
token for identifying authetic  "shard"/"local"/"individual" UTxOs.
Then, we can interact with "shard"/"local"/"individual" UTxOs individually.
We can also destroy the "shard"/"local"/"individual" UTxOs by burning the
appropriate authentication tokens without interacting with the
"central"/"global"/"aggregation" UTxO.
Also, extra "shard"/"local"/"individual" UTxOs can be created by consuming
a previously minted authentication token.

Some examples of this pattern are:
1. CDPCreator and CDP outputs. (CDP script)
2. StakingManager and StakingPosition outputs. (Staking script)
3. Governance and Poll outputs. (Governance script)
4. Governance and iAssetDatum/StabilityPoolDatum outputs (Protocol integration)
....
~
The minting policy below encapsulates the pattern.
-}
{-# INLINEABLE mkAuthTokenPolicy #-}
mkAuthTokenPolicy ::
  Spooky.AssetClass ->
  Spooky.TokenName ->
  () ->
  Spooky.ScriptContext ->
  Bool
mkAuthTokenPolicy nft authTokenName _ ctx =
  traceIfFalse
    "Must consume NFT or token previously minted using this policy"
    ( Spooky.Helpers.hasUnitValue (Spooky.valueSpent info) nft
        || Spooky.Helpers.hasPositiveValue (Spooky.valueSpent info) authAsset
    )
  where
    info :: Spooky.TxInfo
    info = Spooky.scriptContextTxInfo ctx

    authAsset :: Spooky.AssetClass
    authAsset = Spooky.assetClass (Spooky.ownCurrencySymbol ctx) authTokenName

authTokenPolicy :: Value.AssetClass -> Value.TokenName -> V2.MintingPolicy
authTokenPolicy nft authTokenName =
  V2.mkMintingPolicyScript
    ( compiledMintAuthToken
        `PlutusTx.applyCode` PlutusTx.liftCode (Spooky.toSpookyAssetClass nft)
        `PlutusTx.applyCode` PlutusTx.liftCode
          (Spooky.toSpookyTokenName authTokenName)
    )

compiledMintAuthToken ::
  PlutusTx.CompiledCode
    (Spooky.AssetClass -> Spooky.TokenName -> UntypedMintingPolicy)
compiledMintAuthToken =
  Helpers.optimizeUPLC
    $$( PlutusTx.compile
          [||(Spooky.mkUntypedMintingPolicy .) . mkAuthTokenPolicy||]
      )

compiledUntypedMintAuthToken ::
  PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> UntypedMintingPolicy)
compiledUntypedMintAuthToken =
  Helpers.optimizeUPLC $$(PlutusTx.compile [||wrap||])
  where
    wrap ac tn =
      Spooky.mkUntypedMintingPolicy
        ( mkAuthTokenPolicy
            (PlutusTx.unsafeFromBuiltinData ac)
            (PlutusTx.unsafeFromBuiltinData tn)
        )

untypedAuthTokenPolicy :: BuiltinData -> BuiltinData -> V2.MintingPolicy
untypedAuthTokenPolicy nft authTokenName =
  V2.mkMintingPolicyScript
    ( compiledUntypedMintAuthToken
        `PlutusTx.applyCode` PlutusTx.liftCode nft
        `PlutusTx.applyCode` PlutusTx.liftCode authTokenName
    )

untypedAuthTokenPolicyHash :: BuiltinData -> BuiltinData -> V2.MintingPolicyHash
untypedAuthTokenPolicyHash = (PV2.mintingPolicyHash .) . untypedAuthTokenPolicy

-- for checking that hash is the same in Plutus and in CTL
untypedAuthTokenSymbol :: BuiltinData -> BuiltinData -> Value.CurrencySymbol
untypedAuthTokenSymbol = (Value.mpsSymbol .) . untypedAuthTokenPolicyHash

-- serialised for use in CTL
authTokenCTL :: V2.Script
authTokenCTL = V2.fromCompiledCode compiledUntypedMintAuthToken

authTokenPolicyHash ::
  Value.AssetClass -> Value.TokenName -> V2.MintingPolicyHash
authTokenPolicyHash = (PV2.mintingPolicyHash .) . authTokenPolicy

authTokenSymbol :: Value.AssetClass -> Value.TokenName -> Value.CurrencySymbol
authTokenSymbol = (Value.mpsSymbol .) . authTokenPolicyHash
