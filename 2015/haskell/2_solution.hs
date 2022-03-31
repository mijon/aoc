import Data.List
import Data.List.Split

-- part 1

splitDims :: String -> [Int]
splitDims = sort . map read . splitOn "x"

calcArea :: [Int] -> Int
calcArea [a,b,c] = 2*a*b + 2*a*c + 2*b*c + a*b
calcArea [_] = 0

part1 :: [String] -> Int
part1 = sum . map (calcArea . splitDims)

-- part 2

calcRibbon :: [Int] -> Int
calcRibbon [a,b,c] = a+a+b+b + a*b*c
calcRibbon _ = 0

part2 :: [String] -> Int
part2 = sum . map (calcRibbon . splitDims)



-- main

main :: IO ()
main = do
    input <- readFile "../inputs/2_input.txt"
    print $ part1 $ lines input -- 1598415
    print $ part2 $ lines input -- 3812909

