import Data.List.HT
import Data.Maybe

-- Inputs
input = "vzbxkghb"

-- Building a function to increment passwords
alphabet = "abcdefghijklmnopqrstuvwxyz"
myLookup = zip alphabet (rotate 1 alphabet)

nextLetter :: Char -> Char
nextLetter = fromJust . flip lookup myLookup

incrementReversed :: [Char] -> [Char]
incrementReversed ('z':cs) = 'a' : incrementReversed cs
incrementReversed (c:cs) = nextLetter c : cs

increment :: [Char] -> [Char]
increment = reverse . incrementReversed . reverse


-- Checking password requirements
checkPassword :: [Char] -> Bool
checkPassword cs = includesRun cs
                   && noIOL cs
                   && twoPairs cs 0


includesRun :: [Char] -> Bool
includesRun (a:b:c:ds) 
  | null ds = False
  | a <= 'x' && c == nextLetter b && b == nextLetter a = True
  | otherwise = includesRun (b:c:ds)

noIOL :: [Char] -> Bool
noIOL cs = not ('i' `elem` cs 
                 || 'o' `elem` cs
                 || 'l' `elem` cs)

twoPairs :: [Char] -> Int -> Bool
twoPairs cs count
  | count == 2 = True
  | length cs < 2 = False
  | head cs == (head . tail) cs = twoPairs ((tail . tail) cs) (count + 1)
  | otherwise = twoPairs (tail cs) count

-- Generating the next password
nextValidPassword :: [Char] -> [Char]
nextValidPassword = until checkPassword increment


part1 :: [Char]
part1 = nextValidPassword input -- vzbxxyzz

part2 :: [Char]
part2 = nextValidPassword $ increment "vzbxxyzz" -- vzcaabcc
