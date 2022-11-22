---- part 1 workings ----

-- We'll solve part 1 using a finite state machine. The different states
-- involved can be defined at the type level as so:

data StringState = TakeNext       -- the default state
                 | Backslash      -- When just seen a \
                 | BackslashChar  -- when seen a char after a \
                 | BackslashX     -- when seen \x
                 | BackslashX1    -- when seen \x_
                 deriving (Show)

-- The machine transitions between states based on the value of each character.
-- The transitions are arranged as follows
--
--               increment counter
-- ┌──────────────────────────────────┐
-- │                                  │
-- │  any normal char                 │
-- │  increment counter               │
-- │    ┌───┐                   ┌─────┴───────┐
-- │    │   │               ┌───►BackslashChar│
-- │    │   │               │   └─────────────┘
-- │ ┌──▼───┴─┐        ┌────┴────┐
-- ├─►TakeNext├────────►Backslash│
-- │ └──▲───┬─┘        └────┬────┘
-- │    │   │               │   ┌──────────┐
-- │    │   │               └───►BackslashX│
-- │    └───┘                   └─────┬────┘
-- │  specifically "                  │
-- │  don't increment                 │
-- │                            ┌─────▼──────┐
-- └────────────────────────────┤BackslashX1 │
--           increment counter  └────────────┘


-- The problem requires counting the length of the string and so an overall
-- state is a combination of an accumulating counter and the current state 
-- of the machine. 
type State = (Int, StringState)

-- The above diagram is encoded into a function. As there are only a small 
-- number of transitions, it's easy enough to just enumerate all the
-- possibilities.
transition :: State -> Char -> State
transition (x, TakeNext) '"' =  (x,     TakeNext)
transition (x, TakeNext) '\\' = (x,     Backslash)
transition (x, Backslash) 'x' = (x,     BackslashX)
transition (x, BackslashX) _ =  (x,     BackslashX1)
transition (x, BackslashX1) _ = (x + 1, TakeNext)
transition (x, Backslash) _ =   (x + 1, TakeNext)
transition (x, TakeNext) _ =    (x + 1, TakeNext)

-- Running the machine over any particular string is simple a fold.
countPrintedChars :: [Char] -> Int
countPrintedChars = fst . foldl transition (0, TakeNext)

countMemoryChars :: [Char] -> Int
countMemoryChars = length

part1 :: [[Char]] -> Int
part1 xs = sum (map countMemoryChars xs) - sum (map countPrintedChars xs)

---- Part 2 workings ----

-- We could build another machine for this and use the same structure as before
-- but the case here is simpler, effectively " and \ count for two, all other
-- characters count for one. Then we just need to add 2 for the " either side.
encodeCount :: Char -> Int
encodeCount '"' = 2
encodeCount '\\' = 2
encodeCount _ = 1

encodeCounts :: [Char] -> Int
encodeCounts xs = 2 + sum (map encodeCount xs)

part2 :: [[Char]] -> Int
part2 xs = sum (map encodeCounts xs) - sum (map countMemoryChars xs)

---- Main ----

main :: IO ()
main = do
    input <- readFile "../inputs/8_input.txt"
    let strings = lines input
    print $ show $ part1 strings -- 1371
    print $ show $ part2 strings -- 2117

