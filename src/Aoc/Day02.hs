{-# LANGUAGE DeriveAnyClass #-}

module Aoc.Day02 (Day02 (..)) where

import qualified Aoc.Parser as P
import Aoc.Solution (Solution (..))
import Data.Default (Default, def)
import Prelude (pure)

data Day02 = Day02

instance Solution Day02 Int where
  solution1 _ = part1
  solution2 _ = part2

data Command = Command {direction :: Direction, value :: Int}
  deriving (Show)

data Direction
  = Forward
  | -- Note that since you're on a submarine, down and up affect your depth,
    -- and so they have the opposite result of what you might expect.
    Down
  | Up
  deriving (Show)

commandParser :: P.Parser Command
commandParser = do
  direction <- directionParser
  _ <- P.space
  value <- P.decimal
  pure Command {direction, value}

directionParser :: P.Parser Direction
directionParser =
  P.keywords
    [ ("forward", Forward),
      ("down", Down),
      ("up", Up)
    ]

part1 :: Text -> Int
part1 input =
  P.unsafeParse (P.lines commandParser) input
    |> List.foldl simpleEvaluateCommand def
    |> (\(horizontal, depth) -> horizontal * depth)

simpleEvaluateCommand :: Command -> (Int, Int) -> (Int, Int)
simpleEvaluateCommand Command {direction, value} (horizontal, depth) =
  case direction of
    Forward -> (horizontal + value, depth)
    Down -> (horizontal, depth + value)
    Up -> (horizontal, depth - value)

data Navigation = Navigation {horizontal :: Int, depth :: Int, aim :: Int}
  deriving (Show, Generic, Default)

part2 :: Text -> Int
part2 input =
  P.unsafeParse (P.lines commandParser) input
    |> List.foldl evaluateCommand def
    |> (\Navigation {horizontal, depth} -> horizontal * depth)

evaluateCommand :: Command -> Navigation -> Navigation
evaluateCommand Command {direction, value} nav@Navigation {aim, horizontal, depth} =
  case direction of
    Down -> nav {aim = aim + value}
    Up -> nav {aim = aim - value}
    Forward ->
      nav
        { horizontal = horizontal + value,
          depth = depth + aim * value
        }
