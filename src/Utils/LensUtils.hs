{-# LANGUAGE RankNTypes #-}

module Utils.LensUtils where

import Control.Lens (Getting, (^.))
import Control.Monad (join)

infixl 4 /^.

-- | source: https://hackage.haskell.org/package/hw-mquery-0.2.1.0/docs/HaskellWorks-Data-MQuery.html
(/^.) :: Monad m => s -> Getting a s a -> m a
(/^.) a g = return (a ^. g)

infixl 4 >>^.

-- | source: https://hackage.haskell.org/package/hw-mquery-0.2.1.0/docs/HaskellWorks-Data-MQuery.html
(>>^.) :: Monad m => m a -> Getting b a b -> m b
(>>^.) q g = q >>= (/^. g)

infixl 4 ?^.

-- | Like ^. but it can be applied to a Maybe type
(?^.) :: Maybe s -> Getting a s a -> Maybe a
(?^.) = (>>^.)

infixl 4 ?^.?

-- | Like ^. but it can be applied to Maybe type and a lens which returns a Maybe type
(?^.?) :: Maybe s -> Getting (Maybe a) s (Maybe a) -> Maybe a
(?^.?) obj lens = join $ obj ?^. lens