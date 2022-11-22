{-# LANGUAGE OverloadedStrings #-}
import Text.Regex.TDFA

check :: String
check = "(-)?[0-9]+"

getAllNums :: String -> [Int]
getAllNums s = map read $ getAllTextMatches (s =~ check )

part1 :: String -> Int
part1 = sum . getAllNums

main :: IO ()
main = do
    input <- readFile "../inputs/12_input.txt"
    print $ part1 input -- 191164
