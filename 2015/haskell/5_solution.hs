

-- Part 1
checkVowels :: String -> Bool
checkVowels ss = length ( filter (== True) [s `elem` "aeiou" | s <- ss]) >= 3

digraphs :: String -> [String]
digraphs ss
  | length ss > 1 = take 2 ss : digraphs (drop 1 ss)
  | otherwise = []

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

doubleLetters :: [String]
doubleLetters = [x : [x] | x <- alphabet]

checkDoubles :: String -> Bool
checkDoubles ss = or $ [ll `elem` digraphs ss | ll <- doubleLetters]

checkNoDisallowed :: String -> Bool
checkNoDisallowed ss = not $ or $ [ll `elem` digraphs ss | ll <- ["ab", "cd", "pq", "xy"]]

checkNice :: String -> Bool
checkNice x = checkVowels x && checkDoubles x && checkNoDisallowed x

countNice :: (String -> Bool) -> [String] -> Int
countNice checkf ss = length $ filter (== True) $ map checkf ss

-- Part 2

trigraphs :: String -> [String]
trigraphs ss
  | length ss > 2 = take 3 ss : trigraphs (drop 1 ss)
  | otherwise = []

checkNonOverlappingDigraphs :: String -> Bool
checkNonOverlappingDigraphs ss = or [ds !! i == ds !! j | i <- [0..length ds - 1], j <- [i+2..length ds - 1]] where
  ds = digraphs ss

triplets :: [String]
triplets = [[a] ++ [b] ++ [a] | a <- alphabet, b <- alphabet]

checkTriplets :: String -> Bool
checkTriplets ss = or [triplet `elem` trigraphs ss | triplet <- triplets]

checkNice2 :: String -> Bool
checkNice2 x = checkNonOverlappingDigraphs x && checkTriplets x


-- Main
main :: IO ()
main = do
    input <- readFile "../inputs/5_input.txt"
    let strings = lines input
    print $ show $ countNice checkNice strings -- 258
    print $ show $ countNice checkNice2 strings -- 53

