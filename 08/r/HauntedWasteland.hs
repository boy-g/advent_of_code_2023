module Main (main) where


import Debug.Trace (trace)


data PouchOfMaps =
  PouchOfMaps {
    pouchOfMapsInstrs :: [Instr],
    pouchOfMapsNodes  :: [Node]
  }
  deriving (Show)

data Instr =
  InstrLeft |
  InstrRight
  deriving (Show, Eq)

data Node =
  Node {
    nodeLabel :: String,  -- TODO rename "nodeName"
    nodeLeft  :: String,
    nodeRight :: String
  }
  deriving (Show)


main :: IO ()
main = do
  puzzleInput <- getContents
  print $ solvePuzzle $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> PouchOfMaps
parsePuzzleInput puzzleInput = pouchOfMaps
  where
  pouchOfMaps  = PouchOfMaps instructions nodes
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
  nodes        = map parseNode linesOfNodes
  linesOfNodes = drop 2 $ lines puzzleInput

parseNode :: String -> Node
parseNode lineOfNode = node
  where
  node         = Node theNodeLabel theNodeLeft theNodeRight
  theNodeLabel = parseNodeLabel lineOfNode
  theNodeLeft  = parseNodeLeft lineOfNode
  theNodeRight = parseNodeRight lineOfNode

parseNodeLabel :: String -> String
parseNodeLabel lineOfNode = theNodeLabel
  where
  theNodeLabel = head $ words lineOfNode

parseNodeLeft :: String -> String
parseNodeLeft lineOfNode = theNodeLeft
  where
  theNodeLeftWithCruft = words lineOfNode !! 2
  theNodeLeft          = init $ drop 1 theNodeLeftWithCruft

parseNodeRight :: String -> String
parseNodeRight lineOfNode = theNodeRight
  where
  theNodeRightWithCruft = last $ words lineOfNode
  theNodeRight          = init theNodeRightWithCruft

solvePuzzle :: PouchOfMaps -> Integer
solvePuzzle pouchOfMaps = countOfSteps
  where
  pathToEnd    = foldl (stepPath nodes) ["AAA"] instrs
  instrs       = pouchOfMapsInstrs pouchOfMaps
  nodes        = pouchOfMapsNodes pouchOfMaps
  countOfSteps = trace (show pathToEnd) 0  -- TODO length

stepPath :: [Node] -> [String] -> Instr -> [String]
stepPath nodes pathOld instr = pathAppended
  where
  labelCurrent = last pathOld
  labelNext    = calcNextLabel nodes labelCurrent instr
  pathAppended = pathOld ++ [labelNext]

calcNextLabel :: [Node] -> String -> Instr -> String
calcNextLabel nodes labelCurrent instr
  | instr == InstrLeft  = labelLeft
  | instr == InstrRight = labelRight
  where
  labelLeft   = nodeLeft nodeCurrent
  labelRight  = nodeRight nodeCurrent
  nodeCurrent = findNode nodes labelCurrent

findNode :: [Node] -> String -> Node
findNode nodes label = node
  where
  node = head $ filter (isNodeNamed label) nodes

isNodeNamed :: String -> Node -> Bool
isNodeNamed name node =
  nodeLabel node == name
