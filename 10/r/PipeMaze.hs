module Main (main) where


import Data.List   (genericIndex)
import Debug.Trace (trace)


data SketchOfPipes =
  SketchOfPipes [[Char]]

data Coord =
  Coord {
    coordX :: Integer,
    coordY :: Integer
  }
  deriving (Show)


main :: IO ()
main = do
  puzzleInput <- getContents
  print $ solvePuzzle $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> SketchOfPipes
parsePuzzleInput puzzleInput = sketch
  where
  sketch       = SketchOfPipes linesOfPipes
  linesOfPipes = lines puzzleInput

solvePuzzle :: SketchOfPipes -> Integer
solvePuzzle sketch = trace (show pathOfSecond) 0  -- TODO
  where
  coordOfStart  = getStart sketch
  pathOfInitial = [coordOfStart]
  pathOfSecond  = extendPath sketch pathOfInitial

getStart :: SketchOfPipes -> Coord
getStart sketch = start
  where
  (width, height) = getWidthHeight sketch
  xs         = [0..(width - 1)]
  ys         = [0..(height - 1)]
  coordsAll  = [ Coord x y | x <- xs, y <- ys ]
  coordsFilt = filter (isStart sketch) coordsAll
  start      = head coordsFilt

getWidthHeight :: SketchOfPipes -> (Integer, Integer)
getWidthHeight sketch = (width, height)
  where
  SketchOfPipes tiles = sketch
  width               = toInteger $ length $ head tiles
  height              = toInteger $ length tiles

isStart :: SketchOfPipes -> Coord -> Bool
isStart sketch coord = is
  where
  Coord x y           = coord
  SketchOfPipes tiles = sketch
  tile = genericIndex (genericIndex tiles y) x
  is   = tile == 'S'

extendPath :: SketchOfPipes -> [Coord] -> [Coord]
extendPath sketch pathOld = trace (show coordsConnected) pathNew
  where
  coordOld        = last pathOld
  coordsConnected = getConnecteds sketch coordOld
  coordNew        = Coord 0 0  -- TODO
  pathNew         = pathOld ++ [coordNew]  -- TODO

getConnecteds :: SketchOfPipes -> Coord -> (Coord, Coord)
getConnecteds sketch coordOld = coordsPair
  where
  coordsNeighbors = getNeighbors coordOld
  coordsConnected = filter (isAligned sketch coordOld) coordsNeighbors
  coordsPair      = (head coordsConnected, last coordsConnected)

getNeighbors :: Coord -> [Coord]
getNeighbors coord = coordsNeighbors
  where
  coordsNeighbors = []  -- TODO

isAligned :: SketchOfPipes -> Coord -> Coord -> Bool
isAligned sketch coordOrig coordOther = is
  where
  is = True  -- TODO
