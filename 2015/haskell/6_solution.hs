{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.State
import Data.List
import Data.Foldable

import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Text as T hiding (map, foldr)
import Text.Megaparsec.Char.Lexer as L
import Data.Either (rights)

import Data.Text.IO as TIO
-- Types --
data Point = Point Int Int deriving (Show, Eq)

data Instruction = TurnOff | TurnOn | Toggle deriving (Show, Eq)

data InstructionPoint = IP 
    { ins :: Instruction
    , points :: [Point] }
    deriving (Show)

-- This takes two points and generates all points that are in the box
-- defined by the two points
expandSquare :: Point -> Point -> [Point]
expandSquare (Point x y) (Point z w) = [Point a b | a <- [x..z], b <- [y..w]]


applyInstruction :: [Point] -> InstructionPoint -> [Point]
applyInstruction ps (IP ins points)
    | ins == TurnOff = [p | p <- ps, p `notElem` points]
    | ins == TurnOn = ps `union` points
    | ins == Toggle = [p | p <- ps, p `notElem` points] `union` [p | p <- points, p `notElem` ps]


runInstruction :: InstructionPoint -> State [Point] ()
runInstruction ip = do
    currpoints <- get
    put $ applyInstruction currpoints ip

-- This function runs many InstructionPoints and allows us to use (execState)
runInstructions :: [InstructionPoint] -> State [Point] ()
runInstructions = traverse_ runInstruction

-- Parser
-- This parser uses megaparsec
type Parser = Parsec Void Text

pInstruction :: Parser Instruction
pInstruction = choice
  [ TurnOn  <$ string "turn on"
  , TurnOff <$ string "turn off"
  , Toggle  <$ string "toggle" ]

pGridRange :: Parser [Point]
pGridRange  = do
    x1 <- L.decimal
    void (char ',')
    y1 <- L.decimal
    void (string " through ")
    x2 <- L.decimal
    void (char ',')
    y2 <- L.decimal
    return $ expandSquare (Point x1 y1) (Point x2 y2)

pInstructionPoint :: Parser InstructionPoint
pInstructionPoint = do
    instruction <- pInstruction
    void (char ' ')
    ps <- pGridRange
    return $ IP instruction ps


-- Testing data --
-- testCurrPoint = expandSquare (Point 0 0) (Point 3 3)
-- testTurnOff = IP TurnOff (expandSquare (Point 0 0) (Point 2 2))
-- testTurnOn = IP TurnOn (expandSquare (Point 1 1) (Point 4 4))
-- testToggle = IP Toggle (expandSquare (Point 1 1) (Point 4 4))


-- Part 1


main :: IO ()
main = do
    input <- TIO.readFile "../inputs/6_input.readlly_short.txt"
    let
        instructions = Data.List.reverse $ rights $ map (runParser pInstructionPoint "file") $ T.lines input
        final = execState (runInstructions instructions) []
    print $ Data.List.length final

