import Data.ByteString.Char8 (pack, unpack)
import System.Environment (getArgs)
import Crypto.Hash
import Data.List

-- Part 1

md5 :: String -> String
md5 x = show y where
  y = hash w :: Digest MD5
  w = pack x

hashPasses :: Int -> String -> Bool
hashPasses m s = take m s == replicate m '0'

makeHash :: Int -> String -> String
makeHash n s = md5 $ s ++ show n

checkHash :: Int -> Int -> String -> String
checkHash m n s 
  | hashPasses m $ makeHash n s = show n
  | otherwise = checkHash m (n + 1) s


-- Part 2

-- This can be solved by refactoring the part 1 solution to take a variable
-- number of zeros, this was the solution I used, but here's another way

-- main

main :: IO ()
main = do
    print $ checkHash 5 0 "bgvyzdsv" -- 254575
    print $ checkHash 6 0 "bgvyzdsv" -- 1038736
