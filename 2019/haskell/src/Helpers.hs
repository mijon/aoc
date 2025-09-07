module Helpers where

readInput :: String -> IO String
readInput day = readFile $ "/home/michael/projects/aoc/2019/input/" ++ day ++ "_input.txt"
