{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.HT (mapAdjacent)
import Data.Either
import Data.Void
import Control.Monad
import Data.Text (Text, pack)
import Data.Text.IO as TIO

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

-- Testing items
tmpInput = lines "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141"
parsedInput = rights $ map (runParser pDistance "file" . pack) tmpInput

-- Types
data Distance = Distance
    { from :: String
    , to :: String
    , dist :: Int } deriving (Show)

-- Functions
locations :: Distance -> [String]
locations d = [from d, to d]

allLocations :: [Distance] -> [String]
allLocations = nub . join . map locations

-- The distance from a to b should be the same as the distance from b to a (assumed), so
-- this function makes a new `Distance` that reflects this fact
reflectDist :: Distance -> Distance
reflectDist (Distance f t d) = Distance t f d

-- We can easily reflect all the distances and then join them on at the end of the main list
expandDistances :: [Distance] -> [Distance]
expandDistances ds = ds ++ map reflectDist ds

-- If we have a list of distances between every point, and two points, we need only
-- filter the list to get the distance we need out.
distBetween :: [Distance] -> String -> String -> Int
distBetween ds f t = dist $ head $ filter (\d -> (from d == f) && (to d == t)) ds

-- The total distance of a path is then the sum of the distances between the points
pathDistance :: [String] -> [Distance] -> Int
pathDistance ps ds = sum $ mapAdjacent (distBetween ds) ps

-- Parser
type Parser = Parsec Void Text

pDistance :: Parser Distance
pDistance = do
    from <- pWord
    string " to "
    to <- pWord
    string " = "
    dist <- L.decimal
    return $ Distance from to dist

pWord = many alphaNumChar

-- Part 1
part1 :: [String] -> Int
part1 ds = minimum allLengths where
  parsedInput = rights $ map (runParser pDistance "file" . pack) ds
  allDists = expandDistances parsedInput
  allPaths = genAllPaths parsedInput
  allLengths = map (`pathDistance` allDists) allPaths

-- Part 2
part2 :: [String] -> Int
part2 ds = maximum allLengths where
  parsedInput = rights $ map (runParser pDistance "file" . pack) ds
  allDists = expandDistances parsedInput
  allPaths = genAllPaths parsedInput
  allLengths = map (`pathDistance` allDists) allPaths

  
genAllPaths :: [Distance] -> [[String]]
genAllPaths = permutations . allLocations


main :: IO ()
main = do
    input <- Prelude.readFile "../inputs/9_input.txt"
    print $ part1 $ lines input -- 207
    print $ part2 $ lines input -- 804

