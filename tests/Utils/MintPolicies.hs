{-# LANGUAGE TemplateHaskell #-}

module Utils.MintPolicies (authPolicy, authPolicySymbol) where

import Indigo.Data.Token (mkAuthTokenPolicy)
import Indigo.Utils.Helpers (optimizeUPLC)
import Indigo.Utils.Spooky qualified as Spooky
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Model.V2 (TypedPolicy, mkTypedPolicy, scriptCurrencySymbol)
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies
  ( UntypedMintingPolicy,
  )
import PlutusTx qualified
import PlutusTx.Prelude

authPolicy :: Value.AssetClass -> Value.TokenName -> TypedPolicy ()
authPolicy nft authTokenName =
  mkTypedPolicy $
    compiledMintAuthToken
      `PlutusTx.applyCode` PlutusTx.liftCode (Spooky.toSpookyAssetClass nft)
      `PlutusTx.applyCode` PlutusTx.liftCode
        (Spooky.toSpookyTokenName authTokenName)

compiledMintAuthToken ::
  PlutusTx.CompiledCode
    (Spooky.AssetClass -> Spooky.TokenName -> UntypedMintingPolicy)
compiledMintAuthToken =
  optimizeUPLC
    $$( PlutusTx.compile
          [||(Spooky.mkUntypedMintingPolicy .) . mkAuthTokenPolicy||]
      )

authPolicySymbol :: Value.AssetClass -> Value.TokenName -> Ledger.CurrencySymbol
authPolicySymbol = (scriptCurrencySymbol .) . authPolicy
