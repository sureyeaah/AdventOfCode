import System.Environment
import qualified Day1
import qualified Day2

main :: IO ()
main = do
  args <- getArgs
  let name = head args
      input = name ++ ".in"
      solver = getSolver name
  readFile input >>= putStrLn . solver

getSolver :: String -> (String -> String)
getSolver name
  | name == "Day1" = Day1.solver1
  | name == "Day1b" = Day1.solver2
  | name == "Day2" = Day2.solver1
  | name == "Day2b" = Day2.solver2
  | otherwise = error "Not solved yet."
