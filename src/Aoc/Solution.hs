{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Aoc.Solution where

import qualified Aoc.Parser as P
import Prelude (IO)

data Solution = forall a b.
  Solution
  { parser :: P.Parser a,
    solution1 :: a -> b,
    solution2 :: a -> b,
    display :: b -> Text,
    visualize :: Maybe (a -> IO ())
  }
