{-# LANGUAGE RankNTypes #-}

module Utils.ListLenses where

import           Control.Lens.Getter (Getter, to)
import           Relude              ((!!?))

index :: forall a. Int -> Getter [a] (Maybe a)
index i = to (!!? i)
