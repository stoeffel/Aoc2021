module Aoc.Parser
  ( parse,
    lines,
    keywords,
    keychars,
    oneOf,
    csv,
    digitInt,
    fromResult,
    module Control.Applicative,
    module Data.Attoparsec.Text,
  )
where

import Control.Applicative (many, optional, (*>), (<*))
import Data.Attoparsec.Text hiding (Result, parse)
import qualified Data.Char
import Data.Foldable (asum)
import Prelude (Either (Left, Right), fail, fromIntegral, pure)

oneOf :: List (Parser a) -> Parser a
oneOf = asum

parse :: Parser a -> Text -> Result Text a
parse parser input =
  case parseOnly parser input of
    Left err -> Err (Text.fromList err)
    Right result -> Ok result

lines :: Parser a -> Parser (List a)
lines p = many (p <* optional endOfLine)

keywords :: List (Text, a) -> Parser a
keywords =
  List.map (\(x, y) -> pure y <* string x)
    >> oneOf

keychars :: List (Char, a) -> Parser a
keychars =
  List.map (\(x, y) -> pure y <* char x)
    >> oneOf

csv :: Parser a -> Parser (List a)
csv p = sepBy1 p (char ',')

digitInt :: Parser Int
digitInt = map (Data.Char.digitToInt >> fromIntegral) digit

fromResult :: Result Text a -> Parser a
fromResult = \case
  Ok a -> pure a
  Err err -> fail (Text.toList err)
