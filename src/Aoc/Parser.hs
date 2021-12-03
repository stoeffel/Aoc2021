module Aoc.Parser
  ( unsafeParse,
    lines,
    keywords,
    oneOf,
    module Control.Applicative,
    module Data.Attoparsec.Text,
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Foldable (asum)
import qualified Prelude

oneOf = asum

unsafeParse :: Parser a -> Text -> a
unsafeParse parser input =
  case parseOnly parser input of
    Prelude.Left _ -> Debug.todo "Failed to parse input"
    Prelude.Right result -> result

lines :: Parser a -> Parser (List a)
lines p = many (p <* endOfLine)

keywords :: List (Text, a) -> Parser a
keywords =
  List.map (\(x, y) -> Prelude.pure y <* string x)
    >> oneOf
