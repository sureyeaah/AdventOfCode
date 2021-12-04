main :: IO ()
main = interact solve2

solve :: String -> String
solve = show . uncurry (*) . foldl (\(x,y) (x',y') -> (x+x',y+y')) (0,0) . map parse . lines

parse :: String -> (Integer, Integer)
parse xs = case words xs of
  ["forward", x] -> (0, read x)
  ["down", x] -> (read x, 0)
  ["up", x] -> (-(read x), 0)
  _ -> undefined

solve2 :: String -> String
solve2 = show . (\(x,y,_)->x*y) . foldl move (0,0,0) . map parse . lines

move :: (Integer, Integer, Integer) -> (Integer, Integer) -> (Integer, Integer, Integer)
move (x',y',a) (x,0) = (x', y', a + x)
move (x',y',a) (_,y) = (x' + a * y, y' + y, a)
