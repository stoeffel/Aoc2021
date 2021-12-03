{-# LANGUAGE AllowAmbiguousTypes #-}

module Aoc.Helpers where

import Control.Applicative (many, (<*))
import Data.Attoparsec.Text (Parser, decimal, endOfLine, parseOnly, string)
import Data.Foldable (asum)
import Data.Typeable (Typeable, typeOf)
import qualified Prelude

class Typeable a => Solution a where
  solution1 :: a -> Text -> Text
  solution1 _ x = x
  solution2 :: a -> Text -> Text
  solution2 _ x = x
  name :: a -> Text
  name x = Debug.toString (typeOf x)

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
    >> asum

count :: Eq a => a -> List a -> Int
count toCount = List.filter (== toCount) >> List.length
