-- |
--  Author: Benedikt Spies
--
--  Simple parser implementation
module Utils.Parser where

import Control.Applicative (Alternative, empty, (<|>))
import GHC.Unicode (isDigit, isHexDigit)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser p'
    where
      p' input = do
        (a, rest) <- p input
        Just (f a, rest)

instance Applicative Parser where
  pure a = Parser p
    where
      p input = Just (a, input)
  (<*>) (Parser p1) (Parser p2) = Parser p'
    where
      p' input = do
        (f, rest) <- p1 input
        (a, rest') <- p2 rest
        Just (f a, rest')

instance Alternative Parser where
  empty = Parser p
    where
      p _ = Nothing
  (<|>) (Parser p1) (Parser p2) = Parser p
    where
      p input = p1 input <|> p2 input

charPredicateParser :: (Char -> Bool) -> Parser Char
charPredicateParser f = Parser p
  where
    p [] = Nothing
    p (x : xs)
      | (f x) = Just (x, xs)
      | otherwise = Nothing

charParser :: Char -> Parser Char
charParser c = charPredicateParser (c ==)

stringParser :: String -> Parser String
stringParser s = sequenceA $ charParser <$> s

digitParser :: Parser Char
digitParser = charPredicateParser isDigit

hexDigitParser :: Parser Char
hexDigitParser = charPredicateParser isHexDigit

hexStringParser :: Parser String
hexStringParser = oneOrMany hexDigitParser

intParser :: Parser Int
intParser = read <$> oneOrMany digitParser

whiteSpaceParser :: Parser String
whiteSpaceParser = oneOrMany $ charParser ' '

nonWhiteSpaceParser :: Parser String
nonWhiteSpaceParser = oneOrMany $ charPredicateParser (/= ' ')

endParser :: Parser ()
endParser = Parser p
  where
    p [] = Just ((), "")
    p _ = Nothing

oneOrMany :: Parser Char -> Parser String
oneOrMany p =
  ((:) <$> p <*> (oneOrMany p))
    <|> pure <$> p
