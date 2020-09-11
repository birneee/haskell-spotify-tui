-- |
--  Author: Benedikt Spies
--
--  Typeclasses for packing and unpacking Strings
module Utils.StringPack where

import Data.ByteString.Internal (ByteString, packChars, unpackChars)

class Packable a where
  pack :: String -> a

class Unpackable a where
  unpack :: a -> String

instance Packable ByteString where
  pack s = packChars s

instance Unpackable ByteString where
  unpack bs = unpackChars bs
