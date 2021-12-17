module Aoc.Day16Parser (solution) where

import Aoc.Parser ((*>))
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Aoc.System.BITS as BITS
import Prelude (fail, fromIntegral, pure)

solution :: S.Solution
solution =
  S.Solution
    { S.parser = BITS.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

solution1 :: BITS.Packet -> Maybe Int
solution1 packet = Just (BITS.versionSum packet)

solution2 :: BITS.Packet -> Maybe Int
solution2 packet = BITS.evaluate packet
