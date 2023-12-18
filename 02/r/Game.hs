module Main (main) where

import Data.List (stripPrefix)
import Debug.Trace (trace)
import Text.Regex (mkRegex, subRegex)

data Hand = Hand {
  red   :: Integer,
  green :: Integer,
  blue  :: Integer
} deriving (Show)

data Game = Game {
  id    :: Integer,
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
  trace line Game gameId []
  where
    gameId = extractGameId line

extractGameId :: String -> Integer
extractGameId line =
  case (stripPrefix "Game " line) of
    Just lineRemaining ->
      gameId
    Nothing ->
      error $ "trying to get game id of corruct line: " ++ line
  where
    gameId = 1234
