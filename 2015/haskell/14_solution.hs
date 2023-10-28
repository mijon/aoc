
-- import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.List
import Data.Data (DataRep(IntRep))
import Data.Either (rights)
import Data.Maybe


-- Types

data Reindeer = Reindeer { name :: String 
                         , speed :: Int
                         , duration :: Int
                         , rest :: Int }
                         deriving (Show)

  




-- Parsing
type Parser = Parsec Void String

pReindeer :: Parser Reindeer
pReindeer = do
  name <- some letterChar
  string " can fly "
  speed <- read <$> some digitChar
  string " km/s for "
  duration <- read <$> some digitChar
  string " seconds, but then must rest for "
  rest <- read <$> some digitChar
  return $ Reindeer name speed duration rest


-- Solution 
distanceInTime :: Int -> Reindeer -> Int
distanceInTime t r = genDistances r !! (t + 1)

genDistances :: Reindeer -> [Int]
genDistances reindeer = [x + y | y <- cycleStartPoints, x <- cyclePoints] 
  where s = speed reindeer
        d = duration reindeer
        r = rest reindeer 
        cyclePoints = [s * x | x <- [0..d]] ++ replicate (r - 1) (s * d)
        cycleStartPoints = [(s * d) * z | z <- [0..]]


-- genDistances reindeer = [s * x + y | y <- cycle_points, x <- [0..d]] 
--   where s = speed reindeer
--         d = duration reindeer
--         r = rest reindeer 
--         cycle_points = [(r + d) * z | z <- [0..]]

part1 :: Int -> [Reindeer] -> Int
part1 t rs = maximum $ map (distanceInTime t) rs

test_data = [ Reindeer "Comet" 14 10 127
            , Reindeer "Dancer" 16 11 162 ]

test2_data = [Reindeer "Vixen" 8 8 53, Reindeer "Blitzen" 13 4 49, Reindeer "Rudolph" 20 7 132, Reindeer "Cupid" 12 4 43, Reindeer "Donner" 9 5 38, Reindeer "Dasher" 10 4 37, Reindeer "Comet" 3 37 76, Reindeer "Prancer" 9 12 97, Reindeer "Dancer" 37 1 36]

indexOfMax :: Ord a => [a] -> Int
indexOfMax xs = fromJust $ elemIndex (maximum xs) xs


part2 :: Int -> [Reindeer] -> Int
part2 n rs = fst $ maximum $ map (\x -> (length x, head x)) $ group $ sort $ tail $ map indexOfMax $ transpose $ map (take n . genDistances) rs


main :: IO ()
main = do
    input <- rights . map (runParser pReindeer "file") . lines <$> readFile "../inputs/14_input.txt"
    print $ part1 2503 input -- 2655
    print $ part2 2503 input -- 1059
