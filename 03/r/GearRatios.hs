module Main (main) where

readStdin :: IO String
readStdin = getContents

main :: IO ()
main = do
  puzzleInput <- readStdin
  print puzzleInput
