{-# LANGUAGE AllowAmbiguousTypes #-}

module Aoc.Helpers where

import Control.Applicative (many, (<*))
import Data.Attoparsec.Text (Parser, decimal, endOfLine, parseOnly)
import Data.Typeable (Typeable, typeOf)
import qualified Prelude

class Typeable a => Solution a where
  solution1 :: a -> Text -> Text
  solution1 _ x = x
  solution2 :: a -> Text -> Text
  solution2 _ x = x
  name :: a -> Text
  name x = Debug.toString (typeOf x)

intLines :: Text -> List Int
intLines = unsafeParse (lines decimal)

unsafeParse :: Parser a -> Text -> a
unsafeParse parser input =
  case parseOnly parser input of
    Prelude.Left _ -> Debug.todo "Failed to parse input"
    Prelude.Right result -> result

lines :: Parser a -> Parser (List a)
lines p = many (p <* endOfLine)
