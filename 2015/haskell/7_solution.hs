{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split
import Data.Void (Void)
import Data.Either (rights)
import Control.Applicative ((<|>))
import Data.Text (Text, pack)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L


example = ["123 -> x"
          ,"456 -> y"
          ,"x AND y -> d"
          ,"x OR y -> e"
          ,"x LSHIFT 2 -> f"
          ,"y RSHIFT 2 -> g"
          ,"NOT x -> h"
          ,"NOT y -> i"]

---- Types ----
type Wire = String

data Instruction = Instruction 
    { output :: Wire
    , expr   :: Expression 
    } deriving (Show)

data Expression = Value Int
                | Not Wire
                | Or Wire Wire
                | And Wire Wire
                | Lshift Wire Int
                | Rshift Wire Int
                deriving (Show)

---- Parser ----
type Parser = Parsec Void Text

pInstruction :: Parser Instruction
pInstruction = do
    expr <- pExpression
    string " -> "
    output <- pWire
    return $ Instruction output expr

pExpression :: Parser Expression
pExpression = try pNot
          <|> try pAnd
          <|> try pOr
          <|> try pLshift
          <|> try pRshift
          <|> Value <$> pValue

-- These could have been defined in line, but it seemed easier to test
-- having broken them out.
pNot :: Parser Expression
pNot = do
    string "NOT "
    Not <$> pWire

pAnd :: Parser Expression
pAnd = do
    a <- pWire
    string " AND "
    And a <$> pWire

pOr :: Parser Expression
pOr = do
   a <- pWire
   string " OR "
   Or a <$> pWire

pLshift :: Parser Expression
pLshift = do
    w <- pWire
    string " LSHIFT "
    Lshift w <$> pValue

pRshift :: Parser Expression
pRshift = do
    w <- pWire
    string " RSHIFT "
    Lshift w <$> pValue

pWire :: Parser Wire
pWire = many letterChar

pValue :: Parser Int
pValue = read <$> many digitChar

---- Utils ----
isOutput :: Wire -> Instruction -> Bool
isOutput w i = output i == w

inputs :: Expression -> [Wire]
inputs (Value _) = []
inputs (Not w) = [w]
inputs (Or w1 w2) = [w1, w2]
inputs (And w1 w2) = [w1, w2]
inputs (Lshift w _) = [w]
inputs (Rshift w _) = [w]







---- testing ---- 
parsed = rights $ map (runParser pInstruction "file" . pack) example
