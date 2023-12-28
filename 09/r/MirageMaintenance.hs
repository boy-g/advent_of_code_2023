module Main (main) where


import Debug.Trace (trace)


data Report =
  Report {
    reportHistories:: [History]
  }
  deriving (Show)

data History =
  History {
    historyValues :: [Integer]
  }
  deriving (Show)

data Sequences =
  Sequences {
    sequencesValss :: [[Integer]]
  }
  deriving (Show)


main :: IO ()
main = do
  puzzleInput <- getContents
  print $ solvePuzzle $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> Report
parsePuzzleInput puzzleInput = report
  where
  report    = Report histories
  histories = map parseLine $ lines puzzleInput

parseLine :: String -> History
parseLine line = history
  where
  history = History values
  values  = map read $ words line

solvePuzzle :: Report -> Integer
solvePuzzle report = trace (show extrapolated0) 0
  where
  seq0          = historyValues $ head $ reportHistories report
  diffSeqs0     = generateDiffSequences [seq0]
  extrapolated0 = extrapolate diffSeqs0
  -- TODO all lines

generateDiffSequences :: [[Integer]] -> [[Integer]]
generateDiffSequences seqsOld
  | isDiffSequencesDone seqsOld =
    seqsOld
  | otherwise =
    generateDiffSequences $ seqsOld ++ [seqNew]
  where
  seqNew = calcDiffs $ last seqsOld

isDiffSequencesDone :: [[Integer]] -> Bool
isDiffSequencesDone seqs = isDone
  where
  isDone  = seqLast == take (length seqLast) seq0
  seqLast = last seqs
  seq0    = [0, 0..]

calcDiffs :: [Integer] -> [Integer]
calcDiffs valsIn = diffs
  where
  steps = zip valsIn $ tail valsIn
  diffs = map (\(a, b) -> b - a) steps

extrapolate :: [[Integer]] -> [[Integer]]
extrapolate seqsOld = seqsNew
  where
  seqsNew = seqsOld  -- TODO
