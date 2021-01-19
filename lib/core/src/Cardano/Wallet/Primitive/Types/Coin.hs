{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the 'Coin' data type, which represents a quantity of
-- lovelace.
--
module Cardano.Wallet.Primitive.Types.Coin
    (
    -- * Type
      Coin (..)
    , coinQuantity

    -- * Operations
    , sumCoins
    , addCoins
    , distance

    -- * Checks
    , isValidCoin
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Data.Foldable
    ( foldl' )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), fixedF )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

import qualified Data.Text as T

-- | A 'Coin' represents a quantity of lovelace.
--
-- Reminder: 1 ada = 1,000,000 lovelace.
--
newtype Coin = Coin
    { unCoin :: Word64
    }
    deriving stock (Ord, Eq, Generic)
    deriving (Read, Show) via (Quiet Coin)

instance Semigroup Coin where
    -- Word64 doesn't have a default Semigroup instance.
    Coin a <> Coin b = Coin (a + b)

instance Monoid Coin where
    mempty = Coin 0

instance ToText Coin where
    toText (Coin c) = T.pack $ show c

instance FromText Coin where
    fromText = validate <=< (fmap (Coin . fromIntegral) . fromText @Natural)
      where
        validate x
            | isValidCoin x =
                return x
            | otherwise =
                Left $ TextDecodingError "Coin value is out of bounds"

instance NFData Coin

instance Bounded Coin where
    minBound = Coin 0
    maxBound = Coin 45_000_000_000_000_000

instance Buildable Coin where
    build (Coin c) = fixedF @Double 6 (fromIntegral c / 1e6)

isValidCoin :: Coin -> Bool
isValidCoin c = c >= minBound && c <= maxBound

-- | Absolute difference between two coin amounts. The result is never negative.
distance :: Coin -> Coin -> Coin
distance (Coin a) (Coin b) = if a < b then Coin (b - a) else Coin (a - b)

addCoins :: Coin -> Coin -> Coin
addCoins (Coin a) (Coin b) = Coin (a + b)

sumCoins :: Foldable t => t Coin -> Coin
sumCoins = foldl' addCoins (Coin 0)

-- | Compatibility function to use while 'Quantity' is still used in non-API
-- parts of the code.
coinQuantity :: Integral a => Coin -> Quantity n a
coinQuantity (Coin n) = Quantity (fromIntegral n)
