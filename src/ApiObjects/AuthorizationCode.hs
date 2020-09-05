module ApiObjects.AuthorizationCode where

import GHC.Exts (IsString (..))
import Utils.StringUtils (Packable (..), Unpackable (..))

newtype AuthorizationCode = AuthorizationCode String
  deriving (Show, Eq)

instance IsString AuthorizationCode where
  fromString s = pack s

instance Packable AuthorizationCode where
  pack s = AuthorizationCode s

instance Unpackable AuthorizationCode where
  unpack (AuthorizationCode s) = s
