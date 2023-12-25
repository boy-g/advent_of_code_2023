module Main (main) where


data Paper =
  Paper [Record]
  deriving (Show)

data Record =
  Record {
    time     :: Integer,
    distance :: Integer
  }
  deriving (Show)


main :: IO ()
main = do
  puzzleInput <- getContents
  print $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> Paper
parsePuzzleInput puzzleInput =
  Paper records
  where
    times     = parseLine $ head $ lines puzzleInput
    distances = parseLine $ last $ lines puzzleInput
    records   = map recordifyTuple $ zip times distances

recordifyTuple :: (Integer, Integer) -> Record
recordifyTuple (time, distance) =
  Record { time=time, distance=distance }

parseLine :: String -> [Integer]
parseLine line =
  milliseconds
  where
    millisecondsString = tail $ words line
    milliseconds       = map read millisecondsString
