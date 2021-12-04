main :: IO ()
main = interact solve

solve :: String -> String
solve = show . g . map read . lines

f :: [Int] -> Int
f xs = length $ filter (uncurry (<)) $ zip xs (tail xs)

g :: [Int] -> Int
g xs = f [ x + y + z | (x,y,z) <- zip3 xs (drop 1 xs) (drop 2 xs) ]