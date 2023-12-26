module Main (main) where


import Debug.Trace (trace)


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
  print $ solvePuzzle $ parsePuzzleInput puzzleInput

solvePuzzle :: Paper -> Integer
solvePuzzle (Paper records) =
  product numWinss
  where
    numWinss = map findNumWins records

findNumWins :: Record -> Integer
findNumWins record =
  numWins
  where
    distances = permutateDistances record
    numWins   = countWins record distances

countWins :: Record -> [Integer] -> Integer  -- TODO only take "distanceRecord"
countWins record distancesAll =
  numWins
  where
    numWins       = toInteger $ length distancesWins
    distancesWins = filter (isWinner record) distancesAll

isWinner :: Record -> Integer -> Bool
isWinner record distanceCandidate =
  distanceBest < distanceCandidate
  where
    Record {distance=distanceBest} = record

permutateDistances :: Record -> [Integer]  -- TODO only take "timeMax"
permutateDistances record =
  distances
  where
    Record {time=timeMax} = record
    holdTimes             = [0..timeMax]
    distances             = map (calcDistance timeMax) holdTimes

calcDistance :: Integer -> Integer -> Integer
calcDistance timeMax timeHold =
  distance
  where
    distance  = timeDrive * speed
    timeDrive = timeMax - timeHold
    speed     = timeHold

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
