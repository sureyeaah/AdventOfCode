{-# LANGUAGE TypeApplications #-}

import Control.Monad.Trans.State
import Data.List.Split
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

main :: IO ()
main = interact solve

type B = [[Int]]

solve :: String -> String
solve xs =
  let l = lines xs
      nums = map (read @Int) $ splitOn "," (head l)
      parseBoard = map (map (read @Int) . words)
      boards = map (parseBoard . tail) . chunksOf 6 . tail $ l
   in show $ snd $ minimum $ mapMaybe (\b -> evalState (result b nums) . S.fromList . concat $ b) boards

result :: B -> [Int] -> State (S.Set Int) (Maybe (Int, Int))
result _ [] = return Nothing
result board (num : nums) = do
  modify $ S.delete num
  d <- gets $ done board
  sum <- gets $ S.foldl (+) 0
  if d
    then return $ Just (length nums, sum * num)
    else result board nums

done :: B -> S.Set Int -> Bool
done board unmarked =
  or $
    [all (\j -> S.notMember (board !! i !! j) unmarked) [0 .. 4] | i <- [0 .. 4]]
      ++ [all (\j -> S.notMember (board !! j !! i) unmarked) [0 .. 4] | i <- [0 .. 4]]