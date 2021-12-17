module Aoc.Day16Parser (solution) where

import Aoc.Parser ((*>))
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Prelude (fail, fromIntegral, pure)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

parser :: P.Parser Packet
parser = do
  binary <- map Text.concat (P.many1 hexParser)
  parseToParser binary packetParser

hexParser :: P.Parser Text
hexParser =
  P.oneOf
    [ P.char '0' *> pure "0000",
      P.char '1' *> pure "0001",
      P.char '2' *> pure "0010",
      P.char '3' *> pure "0011",
      P.char '4' *> pure "0100",
      P.char '5' *> pure "0101",
      P.char '6' *> pure "0110",
      P.char '7' *> pure "0111",
      P.char '8' *> pure "1000",
      P.char '9' *> pure "1001",
      P.char 'A' *> pure "1010",
      P.char 'B' *> pure "1011",
      P.char 'C' *> pure "1100",
      P.char 'D' *> pure "1101",
      P.char 'E' *> pure "1110",
      P.char 'F' *> pure "1111"
    ]

data Packet = Packet
  { header :: Header,
    body :: Body
  }
  deriving (Eq, Show)

data Header = Header
  { version :: Int,
    typeId :: Int
  }
  deriving (Eq, Show)

data Body
  = LiteralValue Int
  | OperatorBody Operator
  deriving (Eq, Show)

data Operator = Operator
  { opType :: OpType,
    packets :: List Packet
  }
  deriving (Eq, Show)

data OpType = Op15 | Op11 deriving (Eq, Show)

solution1 :: Packet -> Maybe Int
solution1 packet = Just (versionSum packet)

solution2 :: Packet -> Maybe Int
solution2 packet = evaluate packet

packetParser :: P.Parser Packet
packetParser = do
  header <- headerParser
  body <- bodyParser header
  pure Packet {header, body}

headerParser :: P.Parser Header
headerParser = do
  version <- nBitsAsInt 3
  typeId <- nBitsAsInt 3
  pure Header {version, typeId}

bodyParser :: Header -> P.Parser Body
bodyParser Header {typeId} = do
  case typeId of
    4 -> map LiteralValue literalValueParser
    _ -> map OperatorBody operatorBodyParser

literalValueParser :: P.Parser Int
literalValueParser = map bitsToInt chunksParser

operatorBodyParser :: P.Parser Operator
operatorBodyParser = do
  opType <- bit
  case opType of
    '0' -> op15Parser
    _ -> op11Parser

op15Parser :: P.Parser Operator
op15Parser = do
  length <- nBitsAsInt 15
  subBits <- P.take (fromIntegral length)
  packets <- parseToParser subBits (P.many packetParser)
  pure Operator {opType = Op15, packets}

op11Parser :: P.Parser Operator
op11Parser = do
  n <- nBitsAsInt 11
  packets <- P.count (fromIntegral n) packetParser
  pure Operator {opType = Op11, packets}

chunksParser :: P.Parser Text
chunksParser = do
  (prefix, number) <- chunk
  case prefix of
    '0' -> pure number
    '1' -> do
      rest <- chunksParser
      pure (number ++ rest)

chunk :: P.Parser (Char, Text)
chunk = do
  prefix <- bit
  number <- map Text.fromList (P.count 4 bit)
  pure (prefix, number)

nBitsAsInt :: Int -> P.Parser Int
nBitsAsInt n =
  P.count (fromIntegral n) bit
    |> map Text.fromList
    |> map bitsToInt

bit :: P.Parser Char
bit = P.oneOf [P.char '0', P.char '1']

bitsToInt :: Text -> Int
bitsToInt =
  Text.foldl
    ( \x acc ->
        acc * 2 + bitToInt x
    )
    0

bitToInt :: Char -> Int
bitToInt '0' = 0
bitToInt '1' = 1

parseToParser :: Text -> P.Parser a -> P.Parser a
parseToParser text p =
  case P.parse p text of
    Ok a -> pure a
    Err err -> fail (Text.toList err)

versionSum :: Packet -> Int
versionSum Packet {header, body} =
  version header + case body of
    LiteralValue x -> 0
    OperatorBody Operator {packets} ->
      packets
        |> List.map versionSum
        |> List.sum

evaluate :: Packet -> Maybe Int
evaluate Packet {header, body} =
  case body of
    LiteralValue n -> Just n
    OperatorBody op ->
      List.filterMap evaluate (packets op)
        |> typeIdToOp (typeId header)

typeIdToOp :: Int -> (List Int -> Maybe Int)
typeIdToOp typeId =
  case typeId of
    0 -> Just << List.sum
    1 -> Just << List.foldl (*) 1
    2 -> List.minimum
    3 -> List.maximum
    5 -> \case
      (x : y : _) -> Just (if x > y then 1 else 0)
      _ -> Just 0
    6 -> \case
      (x : y : _) -> Just (if x < y then 1 else 0)
      _ -> Just 0
    7 -> \case
      (x : y : _) -> Just (if x == y then 1 else 0)
      _ -> Just 0
    _ -> \_ -> Nothing
