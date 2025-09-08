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

-- Saw this on another repo, I think it's more elegant than the solution above with guards.
calcMassRecursive :: Int -> Int
calcMassRecursive = sum . takeWhile (> 0) . tail . iterate calcMass

---- Appendix ----

-- This was my first attempt for part 2, it works, but it's not as nice as the other way.
calcMassRecursiveAlt :: Int -> Int
calcMassRecursiveAlt m
  | calcMass m <= 0 = 0
  | otherwise = calcMass m + calcMassRecursive (calcMass m)
