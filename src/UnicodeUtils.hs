{-# LANGUAGE UnicodeSyntax #-}

module UnicodeUtils where
import Utils.MaybeUtils((?:))
import Utils.StringUtils (Packable (pack), Unpackable (unpack))
import Control.Lens (Getter)
import Network.HTTP.Types (Status)
import Utils.StatusLenses (code)

infixr 0 ❓
(❓) ∷ Maybe a → a → a
(❓) = (?:)
-- {-# INLINE (❓) #-}

-- pack
infixr 9 📦
(📦) ∷ Packable a ⇒ String → a
(📦) s = pack s
-- {-# INLINE (📦) #-}

-- unpack
infixr 9 📖
(📖) ∷ Unpackable a ⇒ a → String
(📖) s = unpack s
-- {-# INLINE (📖) #-}


-- code = to statusCode
infixr 9 🔢
(🔢) ∷ Getter Status Int
(🔢) = code
-- {-# INLINE (🔢) #-}

