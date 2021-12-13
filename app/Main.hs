{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified Aoc.Day12
import qualified Aoc.Grid as G
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import qualified Data.Text.IO
import qualified System.Environment
import qualified Prelude

main :: Prelude.IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    ["12"] -> do
      input <-
        "test/assets/day12.txt"
          |> Text.toList
          |> Data.Text.IO.readFile
      let parsed = P.parse Aoc.Day12.parser input
      case parsed of
        Err err -> Debug.todo err
        Ok p -> do
          let result = Aoc.Day12.run2 p
          let _ = Debug.log "result" result
          Prelude.pure ()
