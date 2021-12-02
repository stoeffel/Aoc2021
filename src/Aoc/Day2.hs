{-# LANGUAGE DeriveAnyClass #-}

module Aoc.Day2 (Day2 (..)) where

import Aoc.Helpers (Solution (..), keywords, lines, unsafeParse)
import Control.Applicative ((*>))
import Data.Attoparsec.Text (Parser, decimal, space, string)
import Data.Default (Default, def)
import Data.Foldable (asum)
import qualified List
import Prelude (pure)
import qualified Prelude

data Day2 = Day2

instance Solution Day2 where
  solution1 _ = part1 >> Debug.toString
  solution2 _ = part2 >> Debug.toString

data Command = Command {direction :: Direction, value :: Int}
  deriving (Show)

data Direction
  = Forward
  | -- Note that since you're on a submarine, down and up affect your depth,
    -- and so they have the opposite result of what you might expect.
    Down
  | Up
  deriving (Show)

commandParser :: Parser Command
commandParser = do
  direction <- directionParser
  _ <- space
  value <- decimal
  pure Command {direction, value}

directionParser :: Parser Direction
directionParser =
  keywords
    [ ("forward", Forward),
      ("down", Down),
      ("up", Up)
    ]

part1 :: Text -> Int
part1 input =
  unsafeParse (lines commandParser) input
    |> List.foldl
      ( \Command {direction, value} (horizontal, depth) ->
          case direction of
            Forward ->
              (horizontal + value, depth)
            Down ->
              (horizontal, depth + value)
            Up ->
              (horizontal, depth - value)
      )
      def
    |> (\(horizontal, depth) -> horizontal * depth)

data Navigation = Navigation {horizontal :: Int, depth :: Int, aim :: Int}
  deriving (Show, Generic, Default)

part2 :: Text -> Int
part2 input =
  unsafeParse (lines commandParser) input
    |> List.foldl
      ( \Command {direction, value} nav@Navigation {aim, horizontal, depth} ->
          case direction of
            Forward ->
              nav
                { horizontal = horizontal + value,
                  depth = depth + aim * value
                }
            Down ->
              nav {aim = aim + value}
            Up ->
              nav {aim = aim - value}
      )
      def
    |> (\Navigation {horizontal, depth} -> horizontal * depth)
