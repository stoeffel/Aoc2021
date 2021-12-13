{-# LANGUAGE DeriveAnyClass #-}

module Aoc.Day02 (solution) where

import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Data.Default (Default, def)
import Prelude (pure)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

data Command = Command {direction :: Direction, value :: Int}
  deriving (Show)

data Direction
  = Forward
  | -- Note that since you're on a submarine, down and up affect your depth,
    -- and so they have the opposite result of what you might expect.
    Down
  | Up
  deriving (Show)

parser :: P.Parser (List Command)
parser = P.lines commandParser

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

solution1 :: List Command -> Int
solution1 commands =
  commands
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

solution2 :: List Command -> Int
solution2 commands =
  commands
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
