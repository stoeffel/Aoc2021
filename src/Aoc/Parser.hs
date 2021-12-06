module Aoc.Parser
  ( unsafeParse,
    lines,
    keywords,
    oneOf,
    csv,
    module Control.Applicative,
    module Data.Attoparsec.Text,
  )
where

import Control.Applicative (many, (*>), (<*))
import Data.Attoparsec.Text
import Data.Foldable (asum)
import Prelude (Either (Left, Right), pure)

oneOf :: List (Parser a) -> Parser a
oneOf = asum

unsafeParse :: Parser a -> Text -> a
unsafeParse parser input =
  case parseOnly parser input of
    Left _ -> Debug.todo "Failed to parse input"
    Right result -> result

lines :: Parser a -> Parser (List a)
lines p = many (p <* endOfLine)

keywords :: List (Text, a) -> Parser a
keywords =
  List.map (\(x, y) -> pure y <* string x)
    >> oneOf

csv :: Parser a -> Parser (List a)
csv p = sepBy1 p (char ',')
