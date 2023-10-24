import Data.List
import qualified Data.Text as DT

-- part 1

readfloor :: String -> Int
readfloor = sum . map replaceParens

replaceParens :: Char -> Int
replaceParens '(' = 1
replaceParens ')' = -1
replaceParens _ = 0

-- part 2

runningFloor :: String -> [Int]
runningFloor = scanl1 (+) . map replaceParens

findBasement :: [Int] -> Int
findBasement ls = maybe 0 (+1) $ elemIndex (-1) ls

-- main 

main :: IO ()
main = do
    input <- readFile "../inputs/1_input.txt"
    print $ readfloor input
    print $ findBasement $ runningFloor input
