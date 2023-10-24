{-# LANGUAGE OverloadedStrings #-}
import Data.Bits
import Data.Text hiding (head, filter, lines, map)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import GHC.Driver.Session (supportedLanguagesAndExtensions)
import Data.Either
-- import Text.Megaparsec.Byte (letterChar)

-- Types
data Action = WireAssignment Int
            | WireAnd WireName WireName
            | WireOr WireName WireName
            | WireShiftL WireName Int
            | WireShiftR WireName Int
            | WireNot WireName
            deriving Show

type Expression = (WireName, Action)
type Instructions = [Expression]

type WireName = String
getExpression :: WireName -> [Expression] -> Expression
getExpression n es = head $ filter (\x -> fst x == n) es

-- Parsing
type Parser = Parsec Void String

pExpression :: Parser Expression
pExpression = do
  action <- pAction
  string " -> "
  wirename <- pWireName
  return (wirename, action)

pWireName :: Parser WireName
pWireName = many letterChar

pAction :: Parser Action
pAction =   try pWireAnd 
        <|> try pWireOr
        <|> try pWireShiftL
        <|> try pWireShiftR
        <|> try pWireNot
        <|> pWireAssignment

integer :: Parser Int
integer = read <$> many digitChar

pWireAssignment :: Parser Action
pWireAssignment = do
  WireAssignment <$> integer


pWireAnd :: Parser Action
pWireAnd = do
  x <- pWireName
  string " AND "
  y <- pWireName
  return $ WireAnd x y

pWireOr :: Parser Action
pWireOr = do
  x <- pWireName
  string " OR "
  y <- pWireName
  return $ WireOr x y

pWireShiftL :: Parser Action
pWireShiftL = do
  x <- pWireName
  string " LSHIFT "
  y <- integer
  return $ WireShiftL x y


pWireShiftR :: Parser Action
pWireShiftR =  do
  x <- pWireName
  string " RSHIFT "
  y <- integer
  return $ WireShiftR x y

pWireNot :: Parser Action
pWireNot = do
  string "NOT "
  WireNot <$> pWireName



---

wrapAround :: Int -> Int
wrapAround x 
   | x < 0 = 2^16 + x 
   | otherwise = x

-- solve :: WireName -> [Expression] -> Int


resolve :: Expression -> [Expression] -> Int
resolve (s, WireAssignment x) _ = wrapAround x 
resolve (s, WireAnd a b) es = wrapAround $ (.&.) (solve a es) (solve b es)
resolve (s, WireOr a b) es  = wrapAround $ (.|.) (solve a es) (solve b es)
resolve (s, WireShiftL a b) es = wrapAround $ shiftL (solve a es ) b
resolve (s, WireShiftR a b) es = wrapAround $ shiftR (solve a es ) b
resolve (s, WireNot a ) es = wrapAround $ complement (solve a es)

solve :: WireName -> [Expression] -> Int
solve s es = resolve (getExpression s es) es

--- solutions 
-- part1 :: [String] -> Int
-- part1 ds = solve "a" rights $ map (runParser pExpression "file") ds
part1 ds = map (runParser pExpression "file") ds


main :: IO ()
main = do
    input <- lines <$> readFile "../inputs/7_input.txt"
    print $ part1 input
