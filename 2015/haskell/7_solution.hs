import Data.List.Split

example = ["123 -> x"
          ,"456 -> y"
          ,"x AND y -> d"
          ,"x OR y -> e"
          ,"x LSHIFT 2 -> f"
          ,"y RSHIFT 2 -> g"
          ,"NOT x -> h"
          ,"NOT y -> i"]

data Instruction = Instruction { defn :: String
                               , target :: String } deriving
                               (Show)

parseInstruction :: String -> Instruction
parseInstruction s = Instruction d t where
  [d, t] = splitOn " -> " s
