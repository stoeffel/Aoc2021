-- NOTE done by hand
module Aoc.Day23 (solution) where

import qualified Aoc.Parser as P
import qualified Aoc.Solution as S

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

parser :: P.Parser Text
parser = map Text.fromList (P.many P.anyChar)

solution1 :: Text -> Text
solution1 _ = "TODO"

solution2 :: Text -> Text
solution2 _ = "TODO"
