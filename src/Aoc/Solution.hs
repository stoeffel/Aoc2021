{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Aoc.Solution where

import qualified Aoc.Parser as P

data Solution = forall a b.
  Show b =>
  Solution
  { parser :: P.Parser a,
    solution1 :: a -> b,
    solution2 :: a -> b
  }