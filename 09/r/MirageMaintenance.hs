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
solvePuzzle report = solution
  where
  seqsOfPredictions = map (predict . historyValues) $ reportHistories report
  valsOfPredictions = map last seqsOfPredictions
  solution          = sum valsOfPredictions
  -- TODO all lines

predict :: [Integer] -> [Integer]
predict seqIn = seqOut
  where
  seqsOfDiff         = generateDiffSequences [seqIn]
  seqsOfExtrapolated = extrapolateSeqs seqsOfDiff
  seqOut             = head seqsOfExtrapolated

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

extrapolateSeqs :: [[Integer]] -> [[Integer]]
extrapolateSeqs seqsOld = seqsExtrapolated
  where
  seqsExtrapolated = foldr extrapolateSeq [] seqsOld

extrapolateSeq :: [Integer] -> [[Integer]] -> [[Integer]]
extrapolateSeq seqIn [] = appendZero [seqIn]
extrapolateSeq seqIn seqsAcc = seqsExtrapolated
  where
  seqsExtrapolated = seqExtrapolated : seqsAcc
  seqExtrapolated  = seqIn ++ [valNew]
  valNew           = last seqIn + (last $ head seqsAcc)

appendZero :: [[Integer]] -> [[Integer]]
appendZero seqsOld = seqsAppended
  where
  seqsAppended = init seqsOld ++ [seqAppended]
  seqAppended  = last seqsOld ++ [0]
