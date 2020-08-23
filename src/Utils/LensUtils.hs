{-# LANGUAGE RankNTypes #-}
module Utils.LensUtils where

import Control.Lens ((^.), Getting)
import Control.Monad (join)

infixl 4 ?^.
(?^.) :: forall s a. Maybe s -> Getting a s a -> Maybe a
-- |Like ^. but it can be applied to a Maybe type
(?^.) obj lens = case obj of
  Nothing -> Nothing
  Just o -> Just $ o ^. lens

infixl 4 ?^.?
(?^.?) :: forall s a. Maybe s -> Getting (Maybe a) s (Maybe a) -> Maybe a
-- |Like ^. but it can be applied to Maybe type and a lens which returns a Maybe type
(?^.?) obj lens = join $ obj ?^. lens