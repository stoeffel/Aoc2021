module Main (main) where

import qualified Aoc.Day1
import qualified Data.Text.IO
import qualified Expect
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
    [ describe
        "day1"
        [ test "example" <| \() -> do
            input <-
              Data.Text.IO.readFile "test/assets/day1-example-part1.txt"
                |> Expect.fromIO
            Aoc.Day1.part1 input
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/day1-example-part1.hs",
          test "real" <| \() -> do
            input <-
              Data.Text.IO.readFile "test/assets/day1-part1.txt"
                |> Expect.fromIO
            Aoc.Day1.part1 input
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/day1-part1.hs",
          test "example part2" <| \() -> do
            input <-
              Data.Text.IO.readFile "test/assets/day1-example-part2.txt"
                |> Expect.fromIO
            Aoc.Day1.part2 input
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/day1-example-part2.hs",
          test "real part2" <| \() -> do
            input <-
              Data.Text.IO.readFile "test/assets/day1-part2.txt"
                |> Expect.fromIO
            Aoc.Day1.part2 input
              |> Debug.toString
              |> Expect.equalToContentsOf "test/golden-results/day1-part2.hs"
        ]
    ]
