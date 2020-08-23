module ApiObjects.RefreshToken where

import           GHC.Exts          (IsString (..))
import           Utils.StringUtils

newtype RefreshToken = RefreshToken String deriving (Show)

instance IsString RefreshToken where
    fromString s = pack s

instance Packable RefreshToken where
    pack s = RefreshToken s

instance Unpackable RefreshToken where
    unpack (RefreshToken s) = s
