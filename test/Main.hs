{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Aoc.Day1
import qualified Aoc.Day2
import qualified Aoc.Day3
import Aoc.Helpers
import qualified Data.Text.IO
import qualified Expect
import qualified System.Directory as Directory
import Test (Test, describe, test)
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = do
  Test.run tests

tests :: Test
tests =
  describe
    "AoC21"
    [ mkTests Aoc.Day1.Day1,
      mkTests Aoc.Day2.Day2,
      mkTests Aoc.Day3.Day3
    ]

mkTests :: (Solution b) => b -> Test
mkTests solution =
  describe
    (name solution)
    [ mkTest (solution1 solution) (name solution) "example-part1",
      mkTest (solution1 solution) (name solution) "part1",
      mkTest (solution2 solution) (name solution) "example-part2",
      mkTest (solution2 solution) (name solution) "part2"
    ]

mkTest :: (Text -> Text) -> Text -> Text -> Test
mkTest run dayX partX =
  let testName = Text.toLower dayX ++ "-" ++ partX
   in test testName <| \() -> do
        let asset = Text.toList <| "test/assets/" ++ testName ++ ".txt"
        exists <-
          Directory.doesFileExist asset
            |> Expect.fromIO
        if exists
          then do
            input <-
              Data.Text.IO.readFile asset
                |> Expect.fromIO
            run input
              |> Expect.equalToContentsOf ("test/golden-results/" ++ testName ++ ".hs")
          else do
            Expect.fromIO (Data.Text.IO.writeFile asset "TODO")
            Expect.fail "No asset file found (created one)"
