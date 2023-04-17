-- Copied from https://raw.githubusercontent.com/mlabs-haskell/plutus-use-cases/staging/mlabs/src/Mlabs/NFT/Spooky.hs
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Indigo.Utils.Spooky
  ( ScriptHash (..),
    ValidatorHash (..),
    MintingPolicyHash (..),
    PubKeyHash (..),
    toSpookyValidatorHash,
    unSpookyValidatorHash,
    validatorHash,
    toSpookyMintingPolicyHash,
    unSpookyMintingPolicyHash,
    mintingPolicyHash,
    getPubKeyHash,
    toSpookyPubKeyHash,
    unSpookyPubKeyHash,
    PaymentPubKeyHash (..),
    unPaymentPubKeyHash,
    toSpookyPaymentPubKeyHash,
    unSpookyPaymentPubKeyHash,
    DatumHash (..),
    OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
    CurrencySymbol (..),
    toSpookyCurrencySymbol,
    unSpookyCurrencySymbol,
    unCurrencySymbol,
    mpsSymbol,
    currencyMPSHash,
    TokenName (..),
    toSpookyTokenName,
    unSpookyTokenName,
    unTokenName,
    Value (..),
    unSpookyValue,
    flattenValue,
    singleton,
    valueOf,
    lovelaceValueOf,
    symbols,
    adaSymbol,
    adaToken,
    noAdaValue,
    AssetClass (..),
    toSpookyAssetClass,
    unSpookyAssetClass,
    unAssetClass,
    assetClass,
    assetClassValue,
    assetClassValueOf,
    Credential (..),
    StakingCredential (..),
    Address (..),
    addressCredential,
    toSpookyAddress,
    unSpookyAddress,
    scriptHashAddress,
    scriptAddress,
    toValidatorHash,
    mkTypedValidator,
    mkUntypedValidator,
    mkUntypedMintingPolicy,
    TxOutRef (..),
    toSpookyTxOutRef,
    unSpookyTxOutRef,
    txOutRefId,
    txOutRefIdx,
    ScriptPurpose (..),
    TxOut (..),
    txOutAddress,
    txOutValue,
    txOutDatum,
    txOutReferenceScript,
    TxInInfo (..),
    txInInfoOutRef,
    txInInfoResolved,
    TxInfo (..),
    txInfoData,
    txInfoValidRange,
    txInfoSignatories,
    txInfoOutputs,
    txInfoInputs,
    txInfoReferenceInputs,
    txInfoRedeemers,
    txInfoMint,
    txSignedBy,
    findOwnInput,
    getContinuingOutputs,
    ownHash,
    scriptOutputsAt,
    valuePaidTo,
    valueProduced,
    valueSpent,
    pubKeyOutputsAt,
    findDatum,
    ScriptContext (..),
    scriptContextTxInfo,
    scriptContextPurpose,
    ownCurrencySymbol,
    Spooky,
    toSpooky,
    unSpooky,
  )
where

import Control.Monad (guard)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.OpenApi.Schema qualified as OpenApi
#ifdef TypedSpooky
import Data.Typeable (Typeable)
#endif
import GHC.Generics (Generic)
import Ledger
  ( Datum,
    POSIXTimeRange,
    Redeemer,
    TxId,
  )
import Ledger qualified
import Ledger.Crypto qualified as Crypto
import Ledger.Scripts qualified as Scripts
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies
  ( UntypedMintingPolicy,
  )
import Plutus.Script.Utils.V1.Typed.Scripts.Validators
  ( DatumType,
    RedeemerType,
    TypedValidator,
    UntypedValidator,
    unsafeMkTypedValidator,
  )
import Plutus.V1.Ledger.Api (DCert)
import Plutus.V1.Ledger.Credential qualified as Credential
import Plutus.V1.Ledger.Value qualified as Value
import PlutusCore.Default (DefaultUni)
import PlutusTx qualified
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude
import PlutusTx.Spooky
import PlutusTx.These (These (..))
import Unsafe.Coerce (unsafeCoerce)
import Prelude qualified as P

#ifdef TypedSpooky
deriving anyclass instance FromJSON (Spooky a)
deriving anyclass instance ToJSON (Spooky a)
deriving anyclass instance Typeable a => OpenApi.ToSchema (Spooky a)
#endif

newtype ValidatorHash = ValidatorHash
  {getValidatorHash' :: Spooky BuiltinByteString}
  deriving stock (Generic, P.Show)
  deriving newtype
    ( P.Eq,
      P.Ord,
      Eq,
      PlutusTx.Typeable DefaultUni,
      PlutusTx.Lift DefaultUni,
      PlutusTx.ToData,
      PlutusTx.FromData,
      PlutusTx.UnsafeFromData
    )
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

newtype MintingPolicyHash = MintingPolicyHash
  {getMintingPolicyHash' :: Spooky BuiltinByteString}
  deriving stock (Generic, P.Show)
  deriving newtype
    ( P.Eq,
      P.Ord,
      Eq,
      PlutusTx.Typeable DefaultUni,
      PlutusTx.Lift DefaultUni,
      PlutusTx.ToData,
      PlutusTx.FromData,
      PlutusTx.UnsafeFromData
    )
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

newtype ScriptHash = ScriptHash {getScriptHash' :: Spooky BuiltinByteString}
  deriving stock (Generic, P.Show)
  deriving newtype
    ( P.Eq,
      P.Ord,
      Eq,
      PlutusTx.Typeable DefaultUni,
      PlutusTx.Lift DefaultUni,
      PlutusTx.ToData,
      PlutusTx.FromData,
      PlutusTx.UnsafeFromData
    )
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

newtype PubKeyHash = PubKeyHash {getPubKeyHash' :: Spooky BuiltinByteString}
  deriving stock (Generic, P.Show)
  deriving newtype
    ( P.Eq,
      P.Ord,
      Eq,
      PlutusTx.Typeable DefaultUni,
      PlutusTx.Lift DefaultUni,
      PlutusTx.ToData,
      PlutusTx.FromData,
      PlutusTx.UnsafeFromData
    )
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Ord PubKeyHash where
  {-# INLINEABLE compare #-}
  compare h h' = compare (getPubKeyHash h) (getPubKeyHash h')

{-# INLINEABLE toSpookyValidatorHash #-}
toSpookyValidatorHash :: Ledger.ValidatorHash -> ValidatorHash
toSpookyValidatorHash (Ledger.ValidatorHash hash) =
  ValidatorHash . toSpooky $ hash

{-# INLINEABLE unSpookyValidatorHash #-}
unSpookyValidatorHash :: ValidatorHash -> Ledger.ValidatorHash
unSpookyValidatorHash (ValidatorHash hash) =
  Ledger.ValidatorHash . unSpooky $ hash

{-# INLINEABLE validatorHash #-}
validatorHash :: Scripts.Validator -> ValidatorHash
validatorHash = toSpookyValidatorHash . Scripts.validatorHash

{-# INLINEABLE toSpookyMintingPolicyHash #-}
toSpookyMintingPolicyHash :: Ledger.MintingPolicyHash -> MintingPolicyHash
toSpookyMintingPolicyHash (Ledger.MintingPolicyHash hash) =
  MintingPolicyHash . toSpooky $ hash

{-# INLINEABLE unSpookyMintingPolicyHash #-}
unSpookyMintingPolicyHash :: MintingPolicyHash -> Ledger.MintingPolicyHash
unSpookyMintingPolicyHash (MintingPolicyHash hash) =
  Ledger.MintingPolicyHash . unSpooky $ hash

{-# INLINEABLE mintingPolicyHash #-}
mintingPolicyHash :: Scripts.MintingPolicy -> MintingPolicyHash
mintingPolicyHash = toSpookyMintingPolicyHash . Scripts.mintingPolicyHash

{-# INLINEABLE getPubKeyHash #-}
getPubKeyHash :: PubKeyHash -> BuiltinByteString
getPubKeyHash = unSpooky . getPubKeyHash'

{-# INLINEABLE toSpookyPubKeyHash #-}
toSpookyPubKeyHash :: Crypto.PubKeyHash -> PubKeyHash
toSpookyPubKeyHash (Crypto.PubKeyHash hash) = PubKeyHash . toSpooky $ hash

{-# INLINEABLE unSpookyPubKeyHash #-}
unSpookyPubKeyHash :: PubKeyHash -> Crypto.PubKeyHash
unSpookyPubKeyHash (PubKeyHash hash) = Crypto.PubKeyHash . unSpooky $ hash

newtype PaymentPubKeyHash = PaymentPubKeyHash
  {unPaymentPubKeyHash' :: Spooky PubKeyHash}
  deriving stock (Generic, P.Show)
  deriving newtype
    ( P.Eq,
      P.Ord,
      Eq,
      PlutusTx.Typeable DefaultUni,
      PlutusTx.Lift DefaultUni,
      PlutusTx.ToData,
      PlutusTx.FromData,
      PlutusTx.UnsafeFromData
    )
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Ord PaymentPubKeyHash where
  {-# INLINEABLE compare #-}
  compare h h' = compare (unPaymentPubKeyHash h) (unPaymentPubKeyHash h')

{-# INLINEABLE unPaymentPubKeyHash #-}
unPaymentPubKeyHash :: PaymentPubKeyHash -> PubKeyHash
unPaymentPubKeyHash = unSpooky . unPaymentPubKeyHash'

{-# INLINEABLE toSpookyPaymentPubKeyHash #-}
toSpookyPaymentPubKeyHash :: Ledger.PaymentPubKeyHash -> PaymentPubKeyHash
toSpookyPaymentPubKeyHash (Ledger.PaymentPubKeyHash hash) =
  PaymentPubKeyHash . toSpooky . toSpookyPubKeyHash $ hash

{-# INLINEABLE unSpookyPaymentPubKeyHash #-}
unSpookyPaymentPubKeyHash :: PaymentPubKeyHash -> Ledger.PaymentPubKeyHash
unSpookyPaymentPubKeyHash (PaymentPubKeyHash hash) =
  Ledger.PaymentPubKeyHash . unSpookyPubKeyHash . unSpooky $ hash

newtype DatumHash = DatumHash {getDatumHash' :: Spooky BuiltinByteString}
  deriving stock (Generic)
  deriving newtype
    ( P.Eq,
      P.Ord,
      Eq,
      PlutusTx.Typeable DefaultUni,
      PlutusTx.Lift DefaultUni,
      PlutusTx.ToData,
      PlutusTx.FromData,
      PlutusTx.UnsafeFromData
    )
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

newtype CurrencySymbol = CurrencySymbol
  {unCurrencySymbol' :: Spooky BuiltinByteString}
  deriving stock (Generic, P.Show)
  deriving newtype
    ( P.Eq,
      P.Ord,
      Eq,
      PlutusTx.Typeable DefaultUni,
      PlutusTx.Lift DefaultUni,
      PlutusTx.UnsafeFromData,
      PlutusTx.FromData,
      PlutusTx.ToData
    )
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Ord CurrencySymbol where
  {-# INLINEABLE compare #-}
  compare cs cs' = compare (unCurrencySymbol cs) (unCurrencySymbol cs')

{-# INLINEABLE unCurrencySymbol #-}
unCurrencySymbol :: CurrencySymbol -> BuiltinByteString
unCurrencySymbol = unSpooky . unCurrencySymbol'

{-# INLINEABLE toSpookyCurrencySymbol #-}
toSpookyCurrencySymbol :: Ledger.CurrencySymbol -> CurrencySymbol
toSpookyCurrencySymbol (Value.CurrencySymbol cs) =
  CurrencySymbol . toSpooky $ cs

{-# INLINEABLE unSpookyCurrencySymbol #-}
unSpookyCurrencySymbol :: CurrencySymbol -> Ledger.CurrencySymbol
unSpookyCurrencySymbol (CurrencySymbol cs) =
  Value.CurrencySymbol . unSpooky $ cs

{-# INLINEABLE mpsSymbol #-}

-- | The currency symbol of a monetary policy hash
mpsSymbol :: MintingPolicyHash -> CurrencySymbol
mpsSymbol (MintingPolicyHash h) = CurrencySymbol h

{-# INLINEABLE currencyMPSHash #-}

-- | The minting policy hash of a currency symbol
currencyMPSHash :: CurrencySymbol -> MintingPolicyHash
currencyMPSHash (CurrencySymbol h) = MintingPolicyHash h

newtype TokenName = TokenName {unTokenName' :: Spooky BuiltinByteString}
  deriving stock (Generic, P.Show)
  deriving newtype
    ( P.Eq,
      P.Ord,
      Eq,
      PlutusTx.Typeable DefaultUni,
      PlutusTx.Lift DefaultUni,
      PlutusTx.UnsafeFromData,
      PlutusTx.FromData,
      PlutusTx.ToData
    )
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Ord TokenName where
  {-# INLINEABLE compare #-}
  compare tn tn' = compare (unTokenName tn) (unTokenName tn')

{-# INLINEABLE unTokenName #-}
unTokenName :: TokenName -> BuiltinByteString
unTokenName = unSpooky . unTokenName'

{-# INLINEABLE toSpookyTokenName #-}
toSpookyTokenName :: Ledger.TokenName -> TokenName
toSpookyTokenName (Value.TokenName tn) = TokenName . toSpooky $ tn

{-# INLINEABLE unSpookyTokenName #-}
unSpookyTokenName :: TokenName -> Ledger.TokenName
unSpookyTokenName (TokenName tn) = Value.TokenName . unSpooky $ tn

data AssetClass = AssetClass CurrencySymbol TokenName
  deriving stock (P.Eq, P.Ord, Generic, P.Show)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

PlutusTx.unstableMakeIsData ''AssetClass
PlutusTx.makeLift ''AssetClass

instance Eq AssetClass where
  {-# INLINEABLE (==) #-}
  AssetClass c1 t1 == AssetClass c2 t2 = c1 == c2 && t1 == t2

instance Ord AssetClass where
  {-# INLINEABLE compare #-}
  compare ac ac' = compare (unAssetClass ac) (unAssetClass ac')

{-# INLINEABLE unAssetClass #-}
unAssetClass :: AssetClass -> (CurrencySymbol, TokenName)
unAssetClass (AssetClass c t) = (c, t)

{-# INLINEABLE toSpookyAssetClass #-}
toSpookyAssetClass :: Ledger.AssetClass -> AssetClass
toSpookyAssetClass (Value.AssetClass (c, t)) =
  assetClass (toSpookyCurrencySymbol c) (toSpookyTokenName t)

{-# INLINEABLE unSpookyAssetClass #-}
unSpookyAssetClass :: AssetClass -> Ledger.AssetClass
unSpookyAssetClass ac =
  let (c, t) = unAssetClass ac
   in Value.assetClass (unSpookyCurrencySymbol c) (unSpookyTokenName t)

{-# INLINEABLE assetClass #-}
assetClass :: CurrencySymbol -> TokenName -> AssetClass
assetClass = AssetClass

newtype Value = Value {getValue :: Map CurrencySymbol (Map TokenName Integer)}
  deriving stock (Generic)
  deriving newtype (PlutusTx.UnsafeFromData, PlutusTx.FromData, PlutusTx.ToData)

instance P.Semigroup Value where
  (<>) = unionWith (+)

instance Semigroup Value where
  {-# INLINEABLE (<>) #-}
  (<>) = unionWith (+)

instance P.Monoid Value where
  mempty = Value Map.empty

instance Monoid Value where
  {-# INLINEABLE mempty #-}
  mempty = Value Map.empty

instance Group Value where
  {-# INLINEABLE inv #-}
  inv (Value v) = Value ((negate <$>) <$> v)

instance AdditiveSemigroup Value where
  {-# INLINEABLE (+) #-}
  (+) = (<>)

instance AdditiveMonoid Value where
  {-# INLINEABLE zero #-}
  zero = mempty

instance AdditiveGroup Value where
  {-# INLINEABLE (-) #-}
  (-) = unionWith (-)

instance P.Eq Value where
  (==) = eq

instance Eq Value where
  {-# INLINEABLE (==) #-}
  (==) = eq

{-# INLINEABLE unSpookyValue #-}
unSpookyValue :: Value -> Value.Value
unSpookyValue =
  mconcat
    . fmap
      ( \(cs, tn, v) ->
          Value.singleton
            (unSpookyCurrencySymbol cs)
            (unSpookyTokenName tn)
            v
      )
    . flattenValue

{-# INLINEABLE checkPred #-}
checkPred :: (These Integer Integer -> Bool) -> Value -> Value -> Bool
checkPred f l r =
  let inner :: Map TokenName (These Integer Integer) -> Bool
      inner = Map.all f
   in Map.all inner (unionVal l r)

{-# INLINEABLE checkBinRel #-}

-- | Check whether a binary relation holds for value pairs of two 'Value' maps,
--   supplying 0 where a key is only present in one of them.
checkBinRel :: (Integer -> Integer -> Bool) -> Value -> Value -> Bool
checkBinRel f l r =
  let unThese k' = case k' of
        This a -> f a 0
        That b -> f 0 b
        These a b -> f a b
   in checkPred unThese l r

{-# INLINEABLE eq #-}

-- | Check whether one 'Value' is equal to another.
-- See 'Value' for an explanation of how operations on 'Value's work.
eq :: Value -> Value -> Bool
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
eq = checkBinRel (==)

{-# INLINEABLE unionVal #-}

-- | Combine two 'Value' maps
unionVal ::
  Value -> Value -> Map CurrencySymbol (Map TokenName (These Integer Integer))
unionVal (Value l) (Value r) =
  let combined = Map.union l r
      unThese k = case k of
        This a -> This <$> a
        That b -> That <$> b
        These a b -> Map.union a b
   in unThese <$> combined

{-# INLINEABLE unionWith #-}
unionWith :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
unionWith f ls rs =
  let combined = unionVal ls rs
      unThese k' = case k' of
        This a -> f a 0
        That b -> f 0 b
        These a b -> f a b
   in Value (fmap (fmap unThese) combined)

{-# INLINEABLE flattenValue #-}
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
flattenValue (Value v) = do
  (cs, m) <- Map.toList v
  (tn, a) <- Map.toList m
  guard $ a /= 0
  return (cs, tn, a)

{-# INLINEABLE singleton #-}

-- | Make a 'Value' containing only the given quantity of the given currency.
singleton :: CurrencySymbol -> TokenName -> Integer -> Value
singleton c tn i = Value (Map.singleton c (Map.singleton tn i))

{-# INLINEABLE valueOf #-}

-- | Get the quantity of the given currency in the 'Value'.
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
valueOf (Value mp) cur tn =
  case Map.lookup cur mp of
    Nothing -> 0 :: Integer
    Just i -> fromMaybe 0 (Map.lookup tn i)

{-# INLINEABLE lovelaceValueOf #-}
lovelaceValueOf :: Integer -> Value
lovelaceValueOf = singleton adaSymbol adaToken

{-# INLINEABLE symbols #-}

-- | The list of 'CurrencySymbol's of a 'Value'.
symbols :: Value -> [CurrencySymbol]
symbols (Value mp) = Map.keys mp

{-# INLINEABLE assetClassValue #-}

-- | A 'Value' containing the given amount of the asset class.
assetClassValue :: AssetClass -> Integer -> Value
assetClassValue ac i =
  let (c, t) = unAssetClass ac
   in singleton c t i

{-# INLINEABLE assetClassValueOf #-}

-- | Get the quantity of the given 'AssetClass' class in the 'Value'.
assetClassValueOf :: Value -> AssetClass -> Integer
assetClassValueOf v ac =
  let (c, t) = unAssetClass ac
   in valueOf v c t

{-# INLINEABLE adaSymbol #-}
adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol . toSpooky @BuiltinByteString $ ""

{-# INLINEABLE adaToken #-}
adaToken :: TokenName
adaToken = TokenName . toSpooky @BuiltinByteString $ ""

-- | Value without any Ada.
{-# INLINEABLE noAdaValue #-}
noAdaValue :: Value -> Value
noAdaValue v = unionWith (-) v (lovelaceValueOf $ valueOf v adaSymbol adaToken)

data Credential
  = PubKeyCredential (Spooky PubKeyHash)
  | ScriptCredential (Spooky ValidatorHash)
  deriving stock (Generic, P.Show, P.Eq)

PlutusTx.unstableMakeIsData ''Credential
PlutusTx.makeLift ''Credential

instance Eq Credential where
  {-# INLINEABLE (==) #-}
  PubKeyCredential pkh == PubKeyCredential pkh' = pkh == pkh'
  ScriptCredential vh == ScriptCredential vh' = vh == vh'
  _ == _ = False

{-# INLINEABLE unSpookyCredential #-}
unSpookyCredential :: Credential -> Credential.Credential
unSpookyCredential (PubKeyCredential pkh) =
  Credential.PubKeyCredential (unSpookyPubKeyHash $ unSpooky pkh)
unSpookyCredential (ScriptCredential hash) =
  Credential.ScriptCredential (unSpookyValidatorHash $ unSpooky hash)

{-# INLINEABLE toSpookyCredential #-}
toSpookyCredential :: Credential.Credential -> Credential
toSpookyCredential (Credential.PubKeyCredential pkh) =
  PubKeyCredential (toSpooky $ toSpookyPubKeyHash pkh)
toSpookyCredential (Credential.ScriptCredential hash) =
  ScriptCredential (toSpooky $ toSpookyValidatorHash hash)

data StakingCredential
  = StakingHash (Spooky Credential)
  | StakingPtr (Spooky Integer) (Spooky Integer) (Spooky Integer)
  deriving stock (Generic, P.Show, P.Eq)

PlutusTx.unstableMakeIsData ''StakingCredential
PlutusTx.makeLift ''StakingCredential

instance Eq StakingCredential where
  {-# INLINEABLE (==) #-}
  StakingHash c == StakingHash c' = c == c'
  StakingPtr a b c == StakingPtr a' b' c' =
    a == a'
      && b == b'
      && c == c'
  _ == _ = False

{-# INLINEABLE unSpookyStakingCredential #-}
unSpookyStakingCredential :: StakingCredential -> Credential.StakingCredential
unSpookyStakingCredential (StakingHash hash) =
  Credential.StakingHash (unSpookyCredential . unSpooky $ hash)
unSpookyStakingCredential (StakingPtr a b c) =
  Credential.StakingPtr (unSpooky a) (unSpooky b) (unSpooky c)

{-# INLINEABLE toSpookyStakingCredential #-}
toSpookyStakingCredential :: Credential.StakingCredential -> StakingCredential
toSpookyStakingCredential (Credential.StakingHash pkh) =
  StakingHash (toSpooky . toSpookyCredential $ pkh)
toSpookyStakingCredential (Credential.StakingPtr a b c) =
  StakingPtr (toSpooky a) (toSpooky b) (toSpooky c)

data Address = Address
  { addressCredential' :: Spooky Credential,
    addressStakingCredential' :: Spooky (Maybe StakingCredential)
  }
  deriving stock (Generic, P.Show, P.Eq)

PlutusTx.unstableMakeIsData ''Address
PlutusTx.makeLift ''Address

instance Eq Address where
  {-# INLINEABLE (==) #-}
  Address c s == Address c' s' =
    c == c'
      && s == s'

{-# INLINEABLE addressCredential #-}
addressCredential :: Address -> Credential
addressCredential = unSpooky . addressCredential'

{-# INLINEABLE unSpookyAddress #-}
unSpookyAddress :: Address -> Ledger.Address
unSpookyAddress (Address cred sCred) =
  Ledger.Address
    (unSpookyCredential . unSpooky $ cred)
    (fmap unSpookyStakingCredential . unSpooky $ sCred)

{-# INLINEABLE toSpookyAddress #-}
toSpookyAddress :: Ledger.Address -> Address
toSpookyAddress (Ledger.Address cred sCred) =
  Address
    (toSpooky . toSpookyCredential $ cred)
    (toSpooky . fmap toSpookyStakingCredential $ sCred)

{-# INLINEABLE scriptHashAddress #-}

-- | The address that should be used by a transaction output
-- locked by the given validator script hash.
scriptHashAddress :: ValidatorHash -> Address
scriptHashAddress vh =
  Address
    (toSpooky $ ScriptCredential $ toSpooky vh)
    (toSpooky @(Maybe StakingCredential) Nothing)

{-# INLINEABLE scriptAddress #-}

-- | The address that should be used by a transaction output
-- locked by the given validator script.
scriptAddress :: Scripts.Validator -> Address
scriptAddress = scriptHashAddress . validatorHash

{-# INLINEABLE toValidatorHash #-}

-- | The validator hash of the address, if any
toValidatorHash :: Address -> Maybe ValidatorHash
toValidatorHash (Address cred _) = case unSpooky cred of
  ScriptCredential k -> Just (unSpooky k)
  _ -> Nothing

data TxOutRef = TxOutRef
  { txOutRefId' :: Spooky TxId,
    txOutRefIdx' :: Spooky Integer
  }
  deriving stock (Generic, P.Show, P.Eq, P.Ord)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''TxOutRef
PlutusTx.makeLift ''TxOutRef

instance Eq TxOutRef where
  {-# INLINEABLE (==) #-}
  TxOutRef a b == TxOutRef a' b' =
    a == a'
      && b == b'

{-# INLINEABLE txOutRefId #-}
txOutRefId :: TxOutRef -> TxId
txOutRefId = unSpooky . txOutRefId'

{-# INLINEABLE txOutRefIdx #-}
txOutRefIdx :: TxOutRef -> Integer
txOutRefIdx = unSpooky . txOutRefIdx'

{-# INLINEABLE unSpookyTxOutRef #-}
unSpookyTxOutRef :: TxOutRef -> Ledger.TxOutRef
unSpookyTxOutRef (TxOutRef tx idx) =
  Ledger.TxOutRef (unSpooky tx) (unSpooky idx)

{-# INLINEABLE toSpookyTxOutRef #-}
toSpookyTxOutRef :: Ledger.TxOutRef -> TxOutRef
toSpookyTxOutRef (Ledger.TxOutRef tx idx) =
  TxOutRef (toSpooky tx) (toSpooky idx)

data ScriptPurpose
  = Minting (Spooky CurrencySymbol)
  | Spending (Spooky TxOutRef)
  | Rewarding (Spooky StakingCredential)
  | Certifying (Spooky DCert)
  deriving stock (Generic, P.Show, P.Eq)

PlutusTx.unstableMakeIsData ''ScriptPurpose
PlutusTx.makeLift ''ScriptPurpose

instance Eq ScriptPurpose where
  {-# INLINEABLE (==) #-}
  Minting cs == Minting cs' = cs == cs'
  Spending ref == Spending ref' = ref == ref'
  Rewarding sc == Rewarding sc' = sc == sc'
  Certifying cert == Certifying cert' = cert == cert'
  _ == _ = False

data OutputDatum
  = NoOutputDatum
  | OutputDatumHash (Spooky DatumHash)
  | OutputDatum Datum
  deriving stock (P.Eq, Generic)

PlutusTx.unstableMakeIsData ''OutputDatum
PlutusTx.makeLift ''OutputDatum

instance Eq OutputDatum where
  NoOutputDatum == NoOutputDatum = True
  OutputDatumHash dh == OutputDatumHash dh' = dh == dh'
  OutputDatum d == OutputDatum d' = d == d'
  _ == _ = False

data TxOut = TxOut
  { txOutAddress' :: Spooky Address,
    txOutValue' :: Spooky Value,
    txOutDatum' :: Spooky OutputDatum,
    txOutReferenceScript' :: Spooky (Maybe ScriptHash)
  }
  deriving stock (P.Eq, Generic)

PlutusTx.unstableMakeIsData ''TxOut

instance Eq TxOut where
  {-# INLINEABLE (==) #-}
  TxOut a v od rs == TxOut a' v' od' rs' =
    a == a'
      && v == v'
      && od == od'
      && rs == rs'

{-# INLINEABLE txOutAddress #-}
txOutAddress :: TxOut -> Address
txOutAddress = unSpooky . txOutAddress'

{-# INLINEABLE txOutValue #-}
txOutValue :: TxOut -> Value
txOutValue = unSpooky . txOutValue'

{-# INLINEABLE txOutDatum #-}
txOutDatum :: TxOut -> OutputDatum
txOutDatum = unSpooky . txOutDatum'

{-# INLINEABLE txOutReferenceScript #-}
txOutReferenceScript :: TxOut -> Maybe ScriptHash
txOutReferenceScript = unSpooky . txOutReferenceScript'

-- | An input of a pending transaction.
data TxInInfo = TxInInfo
  { txInInfoOutRef' :: Spooky TxOutRef,
    txInInfoResolved' :: Spooky TxOut
  }
  deriving stock (Generic, P.Show, P.Eq)

PlutusTx.unstableMakeIsData ''TxInInfo

instance Eq TxInInfo where
  {-# INLINEABLE (==) #-}
  TxInInfo a b == TxInInfo a' b' =
    a == a'
      && b == b'

{-# INLINEABLE txInInfoOutRef #-}
txInInfoOutRef :: TxInInfo -> TxOutRef
txInInfoOutRef = unSpooky . txInInfoOutRef'

{-# INLINEABLE txInInfoResolved #-}
txInInfoResolved :: TxInInfo -> TxOut
txInInfoResolved = unSpooky . txInInfoResolved'

-- | A pending transaction. This is the view as seen by validator scripts,
-- so some details are stripped out.
data TxInfo = TxInfo
  { -- | Transaction inputs
    txInfoInputs' :: Spooky [TxInInfo],
    -- | Transaction reference inputs
    txInfoReferenceInputs' :: Spooky [TxInInfo],
    -- | Transaction outputs
    txInfoOutputs' :: Spooky [TxOut],
    -- | The fee paid by this transaction.
    txInfoFee' :: Spooky Value,
    -- | The 'Value' minted by this transaction.
    txInfoMint' :: Spooky Value,
    -- | Digests of certificates included in this transaction
    txInfoDCert' :: Spooky [DCert],
    -- | Withdrawals
    txInfoWdrl' :: Spooky (Map StakingCredential Integer),
    -- | The valid range for the transaction.
    txInfoValidRange' :: Spooky POSIXTimeRange,
    -- | Signatures provided with the transaction,
    -- attested that they all signed the tx
    txInfoSignatories' :: Spooky [PubKeyHash],
    txInfoRedeemers' :: Spooky (Map ScriptPurpose Redeemer),
    txInfoData' :: Spooky (Map DatumHash Datum),
    -- | Hash of the pending transaction (excluding witnesses)
    txInfoId' :: Spooky TxId
  }
  deriving stock (Generic, P.Eq)

PlutusTx.unstableMakeIsData ''TxInfo

instance Eq TxInfo where
  {-# INLINEABLE (==) #-}
  TxInfo i ri o f m c w vr s r d tid
    == TxInfo i' ri' o' f' m' c' w' vr' s' r' d' tid' =
      i == i'
        && ri == ri'
        && o == o'
        && f == f'
        && m == m'
        && c == c'
        && w == w'
        && vr == vr'
        && s == s'
        && s == s'
        && r == r'
        && d == d'
        && tid == tid'

{-# INLINEABLE txInfoData #-}
txInfoData :: TxInfo -> Map DatumHash Datum
txInfoData = unSpooky . txInfoData'

{-# INLINEABLE txInfoValidRange #-}
txInfoValidRange :: TxInfo -> POSIXTimeRange
txInfoValidRange = unSpooky . txInfoValidRange'

{-# INLINEABLE txInfoSignatories #-}
txInfoSignatories :: TxInfo -> [PubKeyHash]
txInfoSignatories = unSpooky . txInfoSignatories'

{-# INLINEABLE txInfoOutputs #-}
txInfoOutputs :: TxInfo -> [TxOut]
txInfoOutputs = unSpooky . txInfoOutputs'

{-# INLINEABLE txInfoInputs #-}
txInfoInputs :: TxInfo -> [TxInInfo]
txInfoInputs = unSpooky . txInfoInputs'

{-# INLINEABLE txInfoReferenceInputs #-}
txInfoReferenceInputs :: TxInfo -> [TxInInfo]
txInfoReferenceInputs = unSpooky . txInfoReferenceInputs'

{-# INLINEABLE txInfoRedeemers #-}
txInfoRedeemers :: TxInfo -> Map ScriptPurpose Redeemer
txInfoRedeemers = unSpooky . txInfoRedeemers'

{-# INLINEABLE txInfoMint #-}
txInfoMint :: TxInfo -> Value
txInfoMint = unSpooky . txInfoMint'

{-# INLINEABLE valuePaidTo #-}

-- | Get the total value paid to a public key address by a pending transaction.
valuePaidTo :: TxInfo -> PubKeyHash -> Value
valuePaidTo ptx pkh = mconcat (pubKeyOutputsAt pkh ptx)

{-# INLINEABLE pubKeyOutputsAt #-}

-- | Get the values paid to a public key address by a pending transaction.
pubKeyOutputsAt :: PubKeyHash -> TxInfo -> [Value]
pubKeyOutputsAt pk p = mapMaybe flt (txInfoOutputs p)
  where
    flt tx = case txOutAddress tx of
      Address cred _ -> case unSpooky cred of
        PubKeyCredential pk' ->
          if pk == unSpooky pk' then Just (txOutValue tx) else Nothing
        _ -> Nothing

{-# INLINEABLE scriptOutputsAt #-}

-- | Get the list of 'TxOut' outputs of the pending transaction at
--   a given script address.
scriptOutputsAt :: ValidatorHash -> TxInfo -> [(Datum, Value)]
scriptOutputsAt h p = mapMaybe flt (txInfoOutputs p)
  where
    flt out = case txOutAddress out of
      Address cred _ -> case unSpooky cred of
        ScriptCredential h'
          | h == unSpooky h',
            OutputDatum d <- txOutDatum out ->
              Just (d, txOutValue out)
        _ -> Nothing

{-# INLINEABLE valueSpent #-}

-- | Get the total value of inputs spent by this transaction.
valueSpent :: TxInfo -> Value
valueSpent = foldMap (txOutValue . txInInfoResolved) . txInfoInputs

{-# INLINEABLE valueProduced #-}

-- | Get the total value of outputs produced by this transaction.
valueProduced :: TxInfo -> Value
valueProduced = foldMap txOutValue . txInfoOutputs

{-# INLINEABLE findDatum #-}

-- | Find the data corresponding to a data hash, if there is one
findDatum :: DatumHash -> TxInfo -> Maybe Datum
findDatum dsh tx = Map.lookup dsh (txInfoData tx)

data ScriptContext = ScriptContext
  { scriptContextTxInfo' :: Spooky TxInfo,
    scriptContextPurpose' :: Spooky ScriptPurpose
  }
  deriving stock (Generic, P.Eq)

PlutusTx.unstableMakeIsData ''ScriptContext

{-# INLINEABLE scriptContextTxInfo #-}
scriptContextTxInfo :: ScriptContext -> TxInfo
scriptContextTxInfo = unSpooky . scriptContextTxInfo'

{-# INLINEABLE scriptContextPurpose #-}
scriptContextPurpose :: ScriptContext -> ScriptPurpose
scriptContextPurpose = unSpooky . scriptContextPurpose'

{-# INLINEABLE ownCurrencySymbol #-}
ownCurrencySymbol :: ScriptContext -> CurrencySymbol
ownCurrencySymbol context =
  let purpose = scriptContextPurpose context
   in case purpose of
        Minting cs -> unSpooky cs
        _ -> error ()

{-# INLINEABLE findOwnInput #-}

-- | Find the input currently being validated.
findOwnInput :: ScriptContext -> Maybe TxInInfo
findOwnInput ctx = case scriptContextPurpose ctx of
  Spending ownOutRef ->
    find
      ((ownOutRef ==) . txInInfoOutRef')
      (txInfoInputs $ scriptContextTxInfo ctx)
  _ -> Nothing

{-# INLINEABLE getContinuingOutputs #-}

-- | Get all the outputs that pay to the same script address
-- we are currently spending from, if any.
getContinuingOutputs :: ScriptContext -> [TxOut]
getContinuingOutputs ctx
  | Just input <- findOwnInput ctx =
      filter
        (f $ txOutAddress $ txInInfoResolved input)
        (txInfoOutputs $ scriptContextTxInfo ctx)
  where
    f addr txOut = addr == txOutAddress txOut
getContinuingOutputs _ = traceError "Lf" -- "Can't get any continuing outputs"

{-# INLINEABLE ownHash #-}

-- | Get the hash of the validator script that is currently being validated.
ownHash :: ScriptContext -> ValidatorHash
ownHash ctx = case findOwnInput ctx of
  Just inp -> case addressCredential (txOutAddress $ txInInfoResolved inp) of
    ScriptCredential s -> unSpooky s
    _ -> traceError "No script for ownHash"
  Nothing -> traceError "Can't get ownHash"

{-# INLINEABLE txSignedBy #-}

-- | Check if a transaction was signed by the given public key.
txSignedBy :: TxInfo -> PubKeyHash -> Bool
txSignedBy info k = k `elem` txInfoSignatories info

-- | Converts a custom redeemer from a minting policy function
-- to an untyped minting policy function.
mkUntypedMintingPolicy ::
  forall r.
  PlutusTx.UnsafeFromData r =>
  (r -> ScriptContext -> Bool) ->
  UntypedMintingPolicy
mkUntypedMintingPolicy f r p =
  check $
    f (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData p)

-- | Converts a custom datum and redeemer from a validator function to an
-- untyped validator function.
mkUntypedValidator ::
  forall d r.
  (PlutusTx.UnsafeFromData d, PlutusTx.UnsafeFromData r) =>
  (d -> r -> ScriptContext -> Bool) ->
  UntypedValidator
mkUntypedValidator f d r p =
  check $
    f
      (PlutusTx.unsafeFromBuiltinData d)
      (PlutusTx.unsafeFromBuiltinData r)
      (PlutusTx.unsafeFromBuiltinData p)

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) =
  DatumType a -> RedeemerType a -> ScriptContext -> Bool

-- | Make a 'TypedValidator' from the 'CompiledCode'
-- of a validator script and its wrapper.
mkTypedValidator ::
  -- | Validator script (compiled)
  PlutusTx.CompiledCode (ValidatorType a) ->
  -- | A wrapper for the compiled validator
  PlutusTx.CompiledCode (ValidatorType a -> UntypedValidator) ->
  TypedValidator a
mkTypedValidator vc wrapper =
  let val = Scripts.mkValidatorScript $ wrapper `PlutusTx.applyCode` vc
   in unsafeCoerce $ unsafeMkTypedValidator val
