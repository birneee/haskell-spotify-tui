{-# LANGUAGE UnicodeSyntax #-}
-- |
--  Author: Yang Mao
--
--  Module for unicode synonym to utility functions

module Utils.UnicodeUtils where

import Control.Lens (Getter)
import Network.HTTP.Types (Status)
import Utils.MaybeUtils ((?:))
import Utils.StatusLenses (code)
import Utils.StringPack (Packable (pack), Unpackable (unpack))

infixr 0 ❓
-- | synonym to self defined conditional (ternary) operator
(❓) :: Maybe a -> a -> a
(❓) = (?:)
-- {-# INLINE (❓) #-}


infixr 9 📦
-- | synonym to function pack
(📦) :: Packable a => String -> a
(📦) s = pack s
-- {-# INLINE (📦) #-}



infixr 9 📖
-- | synonym to function unpack
(📖) :: Unpackable a => a -> String
(📖) s = unpack s
-- {-# INLINE (📖) #-}


infixr 9 🔢
-- | synonym to function code
(🔢) :: Getter Status Int
(🔢) = code
-- {-# INLINE (🔢) #-}
