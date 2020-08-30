{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module RawStringText where

import           Language.Haskell.TH (Exp, Q, runQ)
import           Utils.RawString     (rawString, rawStringExp)

buildExpr :: Q Exp
buildExpr = rawStringExp "1\n2\r\n3"

run :: IO Exp
run = runQ buildExpr

splice :: String
splice = $(rawStringExp "1\n2\r\n3")

testWindowsAndUnixLineBreaks :: Bool
testWindowsAndUnixLineBreaks = $(rawStringExp "1\r\n2") == "1\n2"
    && $(rawStringExp "1\n2") == "1\n2"

testQuasiQuoter :: String
testQuasiQuoter = [rawString|1
2 3
                4
5|]
