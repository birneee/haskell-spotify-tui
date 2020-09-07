module Utils.StringUtils where

import Data.ByteString.Internal (ByteString, packChars, unpackChars)
import GHC.Exts (IsString (..))

class Packable a where
  pack :: String -> a

class Unpackable a where
  unpack :: a -> String

instance Packable ByteString where
  pack s = packChars s

instance Unpackable ByteString where
  unpack bs = unpackChars bs
