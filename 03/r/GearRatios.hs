module Main (main) where

import Debug.Trace (trace)

readStdin :: IO String
readStdin = getContents

--TODO data Map = Map [[Char]]

data Coord = Coord {
  x :: Integer,
  y :: Integer
} deriving (Show)

main :: IO ()
main = do
  puzzleInput <- readStdin
  print $ locateAdjacents $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> [[Char]]
parsePuzzleInput puzzleInput =
  lines puzzleInput

locateAdjacents :: [[Char]] -> ([[Char]], [Coord])
locateAdjacents map =
  (map, coordsHasAdjacent)
  where
    height = toInteger $ length map
    width  = toInteger $ length $ head map
    ys = [0..(height-1)] :: [Integer]
    xs = [0..(width-1)]
    coordsAll         =
      [Coord {x=x, y=y} | x<-xs, y<-ys]
    coordsHasAdjacent = filter (hasAdjacentOnMap map) coordsAll

hasAdjacentOnMap :: [[Char]] -> Coord -> Bool
hasAdjacentOnMap map coord =
  True  --TODO
