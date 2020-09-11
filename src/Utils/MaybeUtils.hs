-- |
--  Author: Benedikt Spies
--
--  Utility functions for working with Maybe
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

-- | Pulls a Just value out of a Maybe value.
-- If the Maybe value is Nothing, raises an exception with error.
-- source: https://hackage.haskell.org/package/MissingH-1.4.3.0/docs/src/Data.Maybe.Utils
forceMaybe :: Maybe a -> a
forceMaybe = forceMaybeMsg "forceMaybe: Got Nothing"

-- | Like 'forceMaybe', but lets you customize the error message raised if Nothing is supplied.
-- source: https://hackage.haskell.org/package/MissingH-1.4.3.0/docs/src/Data.Maybe.Utils
forceMaybeMsg :: String -> Maybe a -> a
forceMaybeMsg msg Nothing = error msg
forceMaybeMsg _ (Just x) = x