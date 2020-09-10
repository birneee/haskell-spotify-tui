{-# LANGUAGE UnicodeSyntax #-}
-- enables UnicodeSyntax
module UnicodeUtils where
import Utils.MaybeUtils((?:))
import Utils.StringUtils (Packable (pack), Unpackable (unpack))
import Control.Lens (Getter)
import Network.HTTP.Types (Status)
import Utils.StatusLenses (code)

-- synonym to self defined conditional (ternary) operator
infixr 0 ❓
(❓) ∷ Maybe a → a → a
(❓) = (?:)
-- {-# INLINE (❓) #-}

-- synonym to function pack
infixr 9 📦
(📦) ∷ Packable a ⇒ String → a
(📦) s = pack s
-- {-# INLINE (📦) #-}

-- synonym to function unpack
infixr 9 📖
(📖) ∷ Unpackable a ⇒ a → String
(📖) s = unpack s
-- {-# INLINE (📖) #-}


-- synonym to function code
infixr 9 🔢
(🔢) ∷ Getter Status Int
(🔢) = code
-- {-# INLINE (🔢) #-}

