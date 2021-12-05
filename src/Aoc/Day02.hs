{-# LANGUAGE DeriveAnyClass #-}

module Aoc.Day02 (Day02 (..)) where

import Aoc.Helpers (Solution (..))
import Aoc.Parser
  ( Parser,
    decimal,
    keywords,
    lines,
    oneOf,
    space,
    string,
    unsafeParse,
    (*>),
  )
import Data.Default (Default, def)
import qualified List
import Prelude (pure)
import qualified Prelude

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
  unsafeParse (lines commandParser) input
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
