module Aoc.Helpers where

import Control.Applicative (many, (<*))
import Data.Attoparsec.Text

intLines :: Text -> List Int
intLines input =
  Text.split "\n" input
    |> List.filterMap Text.toInt

unsafeParse :: Parser a -> Text -> a
unsafeParse parser input =
  case parseOnly parser input of
    Prelude.Left _ -> Debug.todo "Failed to parse input"
    Prelude.Right result -> result

lines :: Parser a -> Parser (List a)
lines p = many (p <* endOfLine)
