module Main (main) where

import Data.Char (isDigit)

main :: IO ()
main = do
  puzzleInput <- getContents
  print $ processPuzzleInput puzzleInput

processPuzzleInput puzzleInput =
  --sum processedPuzzleLines
  processedPuzzleLines
  where
    puzzleLines = lines puzzleInput
    processedPuzzleLines = map processPuzzleLine puzzleLines

processPuzzleLine puzzleLine =
  --calibrationValue
  puzzleLine
  where
    digitsLine = filter isDigit puzzleLine
    digitsPair = extractFirstLastPair digitsLine
    calibrationValue = read digitsPair :: Integer

extractFirstLastPair digitsLine =
  [head digitsLine, last digitsLine]
