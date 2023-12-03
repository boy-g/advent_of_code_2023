module Main (main) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

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
    digifiedPuzzleLine = digifyPuzzleLine puzzleLine
    digitsLine         = filter isDigit digifiedPuzzleLine
    digitsPair         = extractFirstLastPair digitsLine
    calibrationValue   = read digitsPair :: Integer

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
  | isDigit nextChar             = consumeDigits (outputLine ++ [nextChar], tail inputLine)
  | isPrefixOf "one"   inputLine = consumeDigits (outputLine ++ "1", drop 3 inputLine)
  | isPrefixOf "two"   inputLine = consumeDigits (outputLine ++ "2", drop 3 inputLine)
  | isPrefixOf "three" inputLine = consumeDigits (outputLine ++ "3", drop 5 inputLine)
  | isPrefixOf "four"  inputLine = consumeDigits (outputLine ++ "4", drop 4 inputLine)
  | isPrefixOf "five"  inputLine = consumeDigits (outputLine ++ "5", drop 4 inputLine)
  | isPrefixOf "six"   inputLine = consumeDigits (outputLine ++ "6", drop 3 inputLine)
  | isPrefixOf "seven" inputLine = consumeDigits (outputLine ++ "7", drop 5 inputLine)
  | isPrefixOf "eight" inputLine = consumeDigits (outputLine ++ "8", drop 5 inputLine)
  | isPrefixOf "nine"  inputLine = consumeDigits (outputLine ++ "9", drop 4 inputLine)
  | otherwise                    = consumeDigits (outputLine, tail inputLine)
  where
    nextChar = head inputLine
