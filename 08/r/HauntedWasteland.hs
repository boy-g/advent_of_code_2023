module Main (main) where

data PouchDeMaps =
  PouchDeMaps {
    pouchDeMapsInstrs :: [Instr],
    pouchDeMapsNodes  :: [Node]
  }
  deriving (Show)

data Instr =
  InstrLeft |
  InstrRight
  deriving (Show)

data Node =
  Node {
    nodeLabel :: String,
    nodeLeft  :: String,
    nodeRight :: String
  }
  deriving (Show)

main :: IO ()
main = do
  puzzleInput <- getContents
  print $ solvePuzzle $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> PouchDeMaps
parsePuzzleInput puzzleInput = pouchDeMaps
  where
  pouchDeMaps  = PouchDeMaps instructions nodes
  instructions = parseInstrs puzzleInput
  nodes        = parseNodes puzzleInput

parseInstrs :: String -> [Instr]
parseInstrs puzzleInput = instructions
  where
  instructions = map parseInstr line0
  line0        = head $ lines puzzleInput

parseInstr :: Char -> Instr
parseInstr 'L' = InstrLeft
parseInstr 'R' = InstrRight
parseInstr _   = error "parseInstr: wtf"

parseNodes :: String -> [Node]
parseNodes puzzleInput = nodes
  where
  nodes        = map parseNode linesDeNodes
  linesDeNodes = drop 2 $ lines puzzleInput

parseNode :: String -> Node
parseNode lineDeNode = node
  where
  node         = Node theNodeLabel theNodeLeft theNodeRight
  theNodeLabel = parseNodeLabel lineDeNode
  theNodeLeft  = parseNodeLeft lineDeNode
  theNodeRight = parseNodeRight lineDeNode

parseNodeLabel :: String -> String
parseNodeLabel lineDeNode = theNodeLabel
  where
  theNodeLabel = head $ words lineDeNode

parseNodeLeft :: String -> String
parseNodeLeft lineDeNode = theNodeLeft
  where
  theNodeLeftWithCruft = words lineDeNode !! 2
  theNodeLeft          = init $ drop 1 theNodeLeftWithCruft

parseNodeRight :: String -> String
parseNodeRight lineDeNode = theNodeRight
  where
  theNodeRightWithCruft = last $ words lineDeNode
  theNodeRight          = init theNodeRightWithCruft

solvePuzzle :: PouchDeMaps -> Integer
solvePuzzle pouchDeMaps = 0  -- TODO
