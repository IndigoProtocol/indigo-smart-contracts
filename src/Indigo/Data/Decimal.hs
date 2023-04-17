-- SPDX-License-Identifier: BUSL-1.1

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO: Study decimal representation more to minimize round-off errors
module Indigo.Data.Decimal
  ( DivideSemigroup (divide),
    DivideMonoid (one),
    OnChainDecimal (OnChainDecimal, getOnChainInt),
    decimalUnit,
    decimalToAdaValue,
    decimal2Integer,
  )
where

import Data.Aeson
import Data.OpenApi.Schema qualified as OpenApi
import Data.Ratio qualified
import Data.Scientific (Scientific)
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Ada qualified as Ada
import PlutusTx qualified
import PlutusTx.Builtins (divideInteger, multiplyInteger)
import PlutusTx.Numeric qualified as PN
import PlutusTx.Prelude
import Prelude qualified as P

{-
Represent Decimal value On Chain.
-}

decimalUnit :: Integer
decimalUnit = 1_000_000

scientificUnit :: Scientific
scientificUnit = P.fromIntegral decimalUnit

decimalToAdaValue :: OnChainDecimal -> Ledger.Value
decimalToAdaValue val = Ada.lovelaceValueOf (getOnChainInt val)

-- | returns the non decimal part of the number
decimal2Integer :: OnChainDecimal -> Integer
decimal2Integer (OnChainDecimal a) = divideInteger a decimalUnit

class DivideSemigroup a where
  divide :: a -> a -> a

class DivideSemigroup a => DivideMonoid a where
  one :: a

newtype Divide a = Divide a

instance Semigroup a => DivideSemigroup (Divide a) where
  {-# INLINEABLE divide #-}
  Divide x `divide` Divide y = Divide (x <> y)

instance Monoid a => DivideMonoid (Divide a) where
  {-# INLINEABLE one #-}
  one = Divide mempty

newtype OnChainDecimal = OnChainDecimal {getOnChainInt :: Integer}
  deriving newtype
    ( P.Enum,
      P.Integral,
      P.Ord,
      P.Eq,
      Enum,
      Ord,
      Eq,
      PN.AdditiveSemigroup,
      PN.AdditiveMonoid,
      PN.AdditiveGroup
    )
  deriving stock (P.Show, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance P.Num OnChainDecimal where
  (+) (OnChainDecimal x) (OnChainDecimal y) = OnChainDecimal (x + y)
  (-) (OnChainDecimal x) (OnChainDecimal y) = OnChainDecimal (x - y)
  (*) (OnChainDecimal x) (OnChainDecimal y) =
    OnChainDecimal (divideInteger (multiplyInteger x y) decimalUnit)
  negate (OnChainDecimal x) = OnChainDecimal (negate x)
  fromInteger x = OnChainDecimal (multiplyInteger x decimalUnit)

  abs (OnChainDecimal x) = OnChainDecimal (abs x)
  signum (OnChainDecimal x) = OnChainDecimal (P.signum x * decimalUnit)

-- | This instance cannot be used on-chain, it's for off-chain purposes only.
instance P.Real OnChainDecimal where
  toRational (OnChainDecimal a) = a Data.Ratio.% decimalUnit

instance PN.MultiplicativeSemigroup OnChainDecimal where
  {-# INLINEABLE (*) #-}
  (*) (OnChainDecimal x) (OnChainDecimal y) =
    OnChainDecimal (divideInteger (multiplyInteger x y) decimalUnit)

instance PN.MultiplicativeMonoid OnChainDecimal where
  {-# INLINEABLE one #-}
  one = OnChainDecimal decimalUnit

instance DivideSemigroup OnChainDecimal where
  {-# INLINEABLE divide #-}
  divide (OnChainDecimal x) (OnChainDecimal y) =
    OnChainDecimal (divideInteger (multiplyInteger x decimalUnit) y)

instance DivideMonoid OnChainDecimal where
  {-# INLINEABLE one #-}
  one = OnChainDecimal decimalUnit

instance ToJSON OnChainDecimal where
  toJSON (OnChainDecimal i) = Number . (P./ scientificUnit) . P.fromIntegral $ i

instance FromJSON OnChainDecimal where
  parseJSON =
    withScientific
      "OnChainDecimal"
      (return . OnChainDecimal . P.round . (P.*) scientificUnit)

PlutusTx.makeLift ''OnChainDecimal
PlutusTx.makeIsDataIndexed ''OnChainDecimal [('OnChainDecimal, 0)]
