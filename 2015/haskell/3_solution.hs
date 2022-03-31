import Control.Monad.Writer
import Data.List

-- Part 1 and Infrastructure
-- We're going to base this solution around the Writer Monad which lets us
-- separate the process of actually moving around from the process of
-- remembering where we went. Essentially, we'll end up writing a function that
-- just handles the moving, in fact it's totally pure as it just says where you
-- will end up given your starting point and a direction to move in. But since
-- we will be in the context of the Writer Monad, we can record where we have
-- been.


-- We'll be using some types to keep track of what we're doing
data Direction = Up | Down | Leftwards | Rightwards deriving Show

data Position = Position 
    { x :: Int
    , y :: Int } deriving (Show, Eq)

-- The pure function that handles the movements
move :: Position -> Direction -> Position
move p Up = Position (x p) (y p + 1)
move p Down = Position (x p) (y p - 1)
move p Leftwards = Position (x p - 1) (y p)
move p Rightwards = Position (x p + 1) (y p)

-- While we could build a parser to do this, since we have a small character
-- set, no need for optionals or anything and a small codomain of types we're
-- mapping to, this simple function approach is sufficient
readDirection :: Char -> Direction
readDirection '^' = Up
readDirection 'v' = Down
readDirection '<' = Leftwards
readDirection '>' = Rightwards

readDirections :: String -> [Direction]
readDirections = map readDirection

-- Note that since we write only on our arrival at a new point, we need to add
-- in the starting position if we want to count the unique places we have been.
countUniqueLocations :: [Position] -> Int
countUniqueLocations ps = length $ nub $ Position 0 0 : ps

-- This function executes a single move inside the Writer context. So this is
-- where we end up writing to the log of where we have been.
processMove :: Position -> Direction -> Writer [Position] Position
processMove p d = tell [nd] >> pure nd where
  nd = move p d

moveSanta :: [Direction] -> Writer [Position] Position
moveSanta = foldM processMove (Position 0 0)


-- Part 2
-- We've already got a lot of machinery that lets us process lists of
-- Directions. We just need a way to split these movements into a list for
-- Santa and another list for RoboSanta: enter uninterleave

-- From DSP
uninterleave :: [a] -> [[a]]
uninterleave = foldr (\x ~[xs, ys] -> [x:ys,xs]) [[],[]]

part2 :: [Direction] -> Int
part2 ds = countUniqueLocations $ concatMap (execWriter . moveSanta) (uninterleave ds)

-- Main
main :: IO ()
main = do
    input <- readFile "../inputs/3_input.txt"
    let dirs = readDirections $ head $ lines input
    let history = execWriter (moveSanta  dirs)
    print $ countUniqueLocations history -- 2592
    print $ part2 dirs -- 2360
