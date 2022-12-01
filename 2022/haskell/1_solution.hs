import Data.List
import Data.List.Split

readTotalCalories :: String -> [Int]
readTotalCalories = map (sum . map read) . splitOn [""] . lines


part1 :: String -> Int
part1 = maximum . readTotalCalories

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . readTotalCalories


main :: IO ()
main = do
    input <- readFile "../input/1_input.txt"
    print $ part1 input -- 70720
    print $ part2 input -- 207148
