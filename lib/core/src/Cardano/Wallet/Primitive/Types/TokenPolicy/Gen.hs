module Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenNameSmallRange
    , genTokenPolicyIdSmallRange
    , shrinkTokenNameSmallRange
    , shrinkTokenPolicyIdSmallRange
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Data.Either
    ( fromRight )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..) )
import Test.QuickCheck
    ( Gen, elements )

import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Token names chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTokenNameSmallRange :: Gen TokenName
genTokenNameSmallRange = elements tokenNames

shrinkTokenNameSmallRange :: TokenName -> [TokenName]
shrinkTokenNameSmallRange name = filter (< name) tokenNames

tokenNames :: [TokenName]
tokenNames = mkTokenName . ("Token" `T.snoc`) <$> ['A' .. 'D']

--------------------------------------------------------------------------------
-- Token policy identifiers chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTokenPolicyIdSmallRange :: Gen TokenPolicyId
genTokenPolicyIdSmallRange = elements tokenPolicies

shrinkTokenPolicyIdSmallRange :: TokenPolicyId -> [TokenPolicyId]
shrinkTokenPolicyIdSmallRange policy = filter (< policy) tokenPolicies

tokenPolicies :: [TokenPolicyId]
tokenPolicies = mkTokenPolicyId <$> ['A' .. 'D']

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

mkTokenName :: Text -> TokenName
mkTokenName t = fromRight reportError $ fromText t
  where
    reportError = error $
        "Unable to generate token name from text: " <> show t

-- The input must be a character in the range [0-9] or [A-Z].
--
mkTokenPolicyId :: Char -> TokenPolicyId
mkTokenPolicyId c
    = fromRight reportError
    $ fromText
    $ T.pack
    $ replicate tokenPolicyIdHexStringLength c
  where
    reportError = error $
        "Unable to generate token policy id from character: " <> show c

tokenPolicyIdHexStringLength :: Int
tokenPolicyIdHexStringLength = 56
