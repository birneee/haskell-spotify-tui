-- |
--  Author: Yang Mao
--
--  Module for unicode synonym to utility functions
 
{-# LANGUAGE UnicodeSyntax #-}
-- enables UnicodeSyntax

module Utils.UnicodeUtils where

import Control.Lens (Getter)
import Network.HTTP.Types (Status)
import Utils.MaybeUtils ((?:))
import Utils.StatusLenses (code)
import Utils.StringPack (Packable (pack), Unpackable (unpack))

-- synonym to self defined conditional (ternary) operator
infixr 0 ❓

(❓) :: Maybe a -> a -> a
(❓) = (?:)

-- {-# INLINE (❓) #-}

-- synonym to function pack
infixr 9 📦

(📦) :: Packable a => String -> a
(📦) s = pack s

-- {-# INLINE (📦) #-}

-- synonym to function unpack
infixr 9 📖

(📖) :: Unpackable a => a -> String
(📖) s = unpack s

-- {-# INLINE (📖) #-}

-- synonym to function code
infixr 9 🔢

(🔢) :: Getter Status Int
(🔢) = code

-- {-# INLINE (🔢) #-}
