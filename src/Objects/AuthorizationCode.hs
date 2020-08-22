module Objects.AuthorizationCode where

import GHC.Exts          (IsString (..))
import Utils.StringUtils

newtype AuthorizationCode = AuthorizationCode String deriving (Show)

instance IsString AuthorizationCode where
    fromString s = pack s

instance Packable AuthorizationCode where
    pack s = AuthorizationCode s

instance Unpackable AuthorizationCode where
    unpack (AuthorizationCode s) = s
