import Data.List (group)

describe :: String -> String
describe x = show (length x) <> [head x]

oneStep :: String -> String
oneStep = concatMap describe . group


input = "1113122113"

-- 360154
part1 :: Int -> String -> Int
part1 n x = length w where
  w = iterate oneStep x !! n

-- 5103798
part2 = part1 50 input
