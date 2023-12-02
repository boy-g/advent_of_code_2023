module Main (main) where

import Data.Char (isDigit)

main :: IO ()
main = do
  puzzleInput <- getContents
  print $ processPuzzleInput puzzleInput

processPuzzleInput puzzleInput =
  sum processedPuzzleLines
  where
    puzzleLines = lines puzzleInput
    processedPuzzleLines = map processPuzzleLine puzzleLines

processPuzzleLine puzzleLine =
  calibrationValue
  where
    digitsLine = filter isDigit puzzleLine
    digitsPair = extractFirstLastPair digitsLine
    calibrationValue = read digitsPair :: Integer

extractFirstLastPair digitsLine =
  [head digitsLine, last digitsLine]
