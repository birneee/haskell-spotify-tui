{-# LANGUAGE TemplateHaskell #-}

module Utils.RawString (rawString, rawStringExp) where

import           Data.List                  (isPrefixOf)
import           Language.Haskell.TH        (Exp, Q)
import           Language.Haskell.TH.Quote  (QuasiQuoter (QuasiQuoter),
                                             quoteDec, quoteExp, quotePat,
                                             quoteType)
import           Language.Haskell.TH.Syntax (Lit (StringL), Exp (LitE))

rawString :: QuasiQuoter
rawString = QuasiQuoter {
    quoteExp = rawStringExp,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}

rawStringExp :: String -> Q Exp
rawStringExp str = return $ LitE $ StringL $ toUnixNewline str

toUnixNewline :: String -> String
toUnixNewline []             = []
toUnixNewline ('\r':'\n':as) = '\n' : as
toUnixNewline (a:as)         = a : (toUnixNewline as)
