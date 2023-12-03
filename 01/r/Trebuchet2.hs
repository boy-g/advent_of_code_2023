module Main (main) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

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
  digifiedPuzzleLine
  where
    digitsLine = filter isDigit puzzleLine
    digitsPair = extractFirstLastPair digitsLine
    calibrationValue = read digitsPair :: Integer
    digifiedPuzzleLine = digifyPuzzleLine puzzleLine

extractFirstLastPair digitsLine =
  [head digitsLine, last digitsLine]

digifyPuzzleLine puzzleLine =
  digifiedPuzzleLine
  where
    (digifiedPuzzleLine, _) = consumeDigits ([], puzzleLine)

consumeDigits :: ([Char], [Char]) -> ([Char], [Char])
consumeDigits (outputLine, []) =
  (outputLine, [])  -- TODO
consumeDigits (outputLine, inputLine)
  | isDigit nextChar             = (outputLine ++ [nextChar], inputLine)  -- TODO
  | isPrefixOf "one" inputLine   = (outputLine ++ "1", inputLine)  -- TODO
  | isPrefixOf "two" inputLine   = consumeDigits (outputLine ++ "2", drop 3 inputLine)  -- TODO
  | isPrefixOf "three" inputLine = (outputLine ++ "3", inputLine)  -- TODO
  | otherwise                    = (outputLine, inputLine)  -- TODO
  where
    nextChar = head inputLine
