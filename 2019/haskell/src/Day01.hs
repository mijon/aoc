module Day01 where

import Helpers

main :: IO ()
main = do
  input <- Helpers.readInput "01"
  print $ part1 input
  print $ part2 input

makeSolver :: (Int -> Int) -> (String -> Int)
makeSolver f = sum . map (f . read) . lines

part1 :: String -> Int
part1 = makeSolver calcMass

part2 :: String -> Int
part2 = makeSolver calcMassRecursive

--- Functions ---

calcMass :: Int -> Int
calcMass m = (m `div` 3) - 2

calcMassRecursive :: Int -> Int
calcMassRecursive m
  | calcMass m <= 0 = 0
  | otherwise = calcMass m + calcMassRecursive (calcMass m)
