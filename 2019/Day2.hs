module Day2 (solver1, solver2)
  where

import Data.Functor (($>))
import Data.List.Split
import Data.Maybe
import Data.List
import qualified Data.Map as M

solver1 :: String -> String
solver1 = show . solve 12 2 . map read . splitOn ","

type State a = (Tape, a)
type Tape = M.Map Int Int

solve :: Int -> Int -> [Int] -> Int
solve a b xs = runTape (makeTape xs', Just 0) M.! 0
  where
    xs' = [head xs, a, b] ++ drop 3 xs


makeTape :: [Int] -> Tape
makeTape = M.fromList . zip [0..]

runTape :: State (Maybe Int) -> Tape
runTape state@(tape, Just x) = runTape . transition $ (tape, x)
runTape (tape, Nothing) = tape

transition :: State Int -> State (Maybe Int)
transition (tape, val) = (fromMaybe tape tape', nextPos)
  where
    tape' = Just M.insert <*> store <*> newVal <*> Just tape
    nextPos = newVal $> val + 4 
    newVal = oper <*> a <*> b
    oper = case op of
      Just 1 -> Just (+)
      Just 2 -> Just (*)
      _ -> Nothing
    op = M.lookup val tape
    a = M.lookup (val+1) tape >>= (\x -> M.lookup x tape)
    b = M.lookup (val+2) tape >>= (\x -> M.lookup x tape)
    store = M.lookup (val+3) tape

solver2 :: String -> String
solver2 = show . solve2. map read . splitOn ","

solve2 :: [Int] -> Int
solve2 xs = 100 * noun + verb
  where n = length xs
        all = [(solve i j xs, i, j) | i <- [0..(n-1)], j <- [0..(n-1)]]
        (_, noun, verb) = head . filter (\(x,_,_) -> x == 19690720) $ all
