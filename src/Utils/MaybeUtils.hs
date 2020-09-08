module Utils.MaybeUtils where

import Data.Maybe (fromMaybe)

-- | Maybe get the 'Right' side of an 'Either'.
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

-- | An infix form of 'fromMaybe' with arguments flipped.
(?:) :: Maybe a -> a -> a
maybeA ?: b = fromMaybe b maybeA
{-# INLINEABLE (?:) #-}

infixr 0 ?: