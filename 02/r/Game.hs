module Main (main) where

import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Text.Regex (mkRegex, subRegex)

data Hand = Hand {
  red   :: Integer,
  green :: Integer,
  blue  :: Integer
} deriving (Show)

data Game = Game {
  id    :: Integer,  -- TODO rename "gameId"?
  hands :: [Hand]
} deriving (Show)

readStdin :: IO String
readStdin = getContents

main :: IO ()
main =
  do
    puzzleInput <- readStdin
    print $ parseInput puzzleInput

parseInput :: String -> [Game]
parseInput input =
  map parseLine inputLines
  where
    inputLines = lines input

parseLine :: String -> Game
parseLine line =
  trace line Game gameId gameHands
  where
    gameId    = parseGameId line
    gameHands = parseGameHands line

parseGameId :: String -> Integer
parseGameId line =
  gameId
  where
    lineWoHead       = subRegex (mkRegex "Game ") line ""
    lineWoHeadWoTail = subRegex (mkRegex ":.*") lineWoHead ""
    gameId           = read lineWoHeadWoTail

parseGameHands :: String -> [Hand]
parseGameHands line =
  trace (show hands) []
  where
    handsLine = subRegex (mkRegex ".*:") line ""
    handLines = splitOn ";" handsLine
    hands = map parseGameHand handLines

parseGameHand :: String -> Hand
parseGameHand handLine =
  trace handLine Hand {red = red, green = green, blue = blue}
  where
    red   = 0
    green = 0
    blue  = 0
