module Objects.AccessToken where

import           GHC.Exts          (IsString (..))
import           Utils.StringUtils

newtype AccessToken = AccessToken String deriving (Show)

instance IsString AccessToken where
    fromString s = pack s

instance Packable AccessToken where
    pack s = AccessToken s

instance Unpackable AccessToken where
    unpack (AccessToken s) = s
