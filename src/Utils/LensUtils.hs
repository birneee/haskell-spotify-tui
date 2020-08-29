{-# LANGUAGE RankNTypes #-}
module Utils.LensUtils where

import Control.Lens ((^.), Getting)
import Control.Monad (join)

infixl 4 /^.
(/^.) :: Monad m => s -> Getting a s a -> m a
-- | source: https://hackage.haskell.org/package/hw-mquery-0.2.1.0/docs/HaskellWorks-Data-MQuery.html
(/^.) a g = return (a ^. g)

infixl 4 >>^.
(>>^.) :: Monad m => m a -> Getting b a b -> m b
-- | source: https://hackage.haskell.org/package/hw-mquery-0.2.1.0/docs/HaskellWorks-Data-MQuery.html
(>>^.) q g = q >>= (/^. g)

infixl 4 ?^.
(?^.) :: Maybe s -> Getting a s a -> Maybe a
-- |Like ^. but it can be applied to a Maybe type
(?^.) = (>>^.)

infixl 4 ?^.?
(?^.?) :: Maybe s -> Getting (Maybe a) s (Maybe a) -> Maybe a
-- |Like ^. but it can be applied to Maybe type and a lens which returns a Maybe type
(?^.?) obj lens = join $ obj ?^. lens