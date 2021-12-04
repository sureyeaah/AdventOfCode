import Control.Monad.State
import Data.List (maximumBy)

main :: IO ()
main = interact solve2

solve :: String -> String
solve xs =
  let c = foldr count (repeat (0, 0)) $ lines xs
   in show $ gamma c * epsilon c

solve2 :: String -> String
solve2 xs =
  let nums = lines xs
      run fn = fromBin $ head $ execState (f fn 0) nums
   in show $ run (<=) * run (>)

count :: String -> [(Int, Int)] -> [(Int, Int)]
count xs c = [if b == '0' then (x + 1, y) else (x, y + 1) | (b, (x, y)) <- zip xs c]

gamma :: [(Int, Int)] -> Int
gamma = foldl (\acc (x, y) -> acc * 2 + (if y >= x then 1 else 0)) 0

epsilon :: [(Int, Int)] -> Int
epsilon = foldl (\acc (x, y) -> acc * 2 + (if x >= y then 0 else 1)) 0

fromBin :: String -> Int
fromBin = foldl (\acc c -> acc * 2 + (if c == '1' then 1 else 0)) 0

asString :: [Bool] -> String
asString = map (\x -> if x then '1' else '0')

f :: (Int -> Int -> Bool) -> Int -> State [String] ()
f fn idx = do
  st <- get
  let s = asString $ map (uncurry fn) $ foldr count (repeat (0, 0)) st
  if length st > 1
    then do
      modify $ filter (\s' -> s' !! idx == s !! idx)
      f fn (idx + 1)
    else pure ()