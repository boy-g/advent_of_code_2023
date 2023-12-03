module Main (main) where

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
parseInput _ = [Game 0 []]
