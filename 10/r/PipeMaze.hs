module Main (main) where


data SketchOfPipes =
  SketchOfPipes [[Char]]
  deriving (Show)


main :: IO ()
main = do
  puzzleInput <- getContents
  print $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> SketchOfPipes
parsePuzzleInput puzzleInput = sketch
  where
  sketch       = SketchOfPipes linesOfPipes
  linesOfPipes = lines puzzleInput
