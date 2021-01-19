{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Wallet.Primitive.Types.TokenPolicy
    (
      -- * Token Policies
      TokenPolicyId (..)

      -- * Token Names
    , TokenName (..)

      -- * Token Metadata
    , AssetMetadata (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( (>=>) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )
import Quiet
    ( Quiet (..) )

import qualified Data.Text.Encoding as T

-- | Token policy identifiers, represented by the hash of the monetary policy
-- script.
newtype TokenPolicyId =
    -- | Construct a 'TokenPolicyId' without any validation.
    UnsafeTokenPolicyId { unTokenPolicyId :: Hash "TokenPolicy" }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenPolicyId)

instance NFData TokenPolicyId

instance Buildable TokenPolicyId where
    build = build . toText . unTokenPolicyId

instance FromJSON TokenPolicyId where
    parseJSON = parseJSON >=> either (fail . show) pure . fromText

instance ToJSON TokenPolicyId where
    toJSON = toJSON . toText

instance ToText TokenPolicyId where
    toText = toText . unTokenPolicyId

instance FromText TokenPolicyId where
    fromText = fmap UnsafeTokenPolicyId . fromText

-- | Token names, defined by the monetary policy script.
newtype TokenName =
    -- | Construct a 'TokenName' without any validation.
    UnsafeTokenName { unTokenName :: ByteString }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenName)

instance NFData TokenName

instance Buildable TokenName where
    build = build . toText

instance FromJSON TokenName where
    parseJSON = parseJSON >=> either (fail . show) pure . fromText

instance ToJSON TokenName where
    toJSON = toJSON . toText

instance ToText TokenName where
    toText = T.decodeLatin1 . convertToBase Base16 . unTokenName

instance FromText TokenName where
    fromText = bimap err UnsafeTokenName . convertFromBase Base16 . T.encodeUtf8
      where
        err msg = TextDecodingError $
            "TokenName hash is not hex-encoded: " ++ msg

-- | Information about an asset, from a source external to the chain.
newtype AssetMetadata = AssetMetadata
    { name :: Text
    } deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet AssetMetadata)

instance NFData AssetMetadata
