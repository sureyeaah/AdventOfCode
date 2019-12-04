module Day1 (solver1, solver2)
  where
solver1 :: String -> String
solver1 = show . solve . map read . lines

solve :: [Int] -> Int
solve = sum . map fuel

fuel :: Int -> Int
fuel x = (x `div` 3) - 2

solver2 :: String -> String
solver2 = show . solve2 . map read . lines

solve2 :: [Int] -> Int
solve2 = sum . map (fuel2 0)

fuel2 :: Int -> Int -> Int
fuel2 tot f = if nxtFuel > 0 then fuel2 (tot + nxtFuel) nxtFuel else tot
  where nxtFuel = fuel f
