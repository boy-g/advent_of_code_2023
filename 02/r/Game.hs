module Main (main) where

import Debug.Trace (trace)
--import Text.Regex (subRegex)

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
main = do
  puzzleInput <- readStdin
  print $ parseInput puzzleInput

parseInput :: String -> [Game]
parseInput input =
  map parseLine inputLines
  where
    inputLines = lines input

parseLine :: String -> Game
parseLine line = trace line $ Game 0 []
