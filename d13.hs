import DayZero
import Data.List.Split



main :: IO ()
main = do
    (t:b:[]) <- splitFile "input_d13.txt" "\n"
    let time = read t
    let buses = splitOn "," b
    print time
    print $ (\x -> fst x * snd x) $ minimum $ map (\x -> (-(time `mod` x)+x, x)) $ map (read :: String -> Int) $ filter (/= "x") buses
    let p2Input = map (\(x, y) -> (x, read y :: Int)) $ filter ((/= "x") . snd) $ zip [1..] buses
    print $ run 1 1 p2Input

run :: Int -> Int -> [(Int,Int)] -> Int
run t _ [] = t + 1
run t q xs@((i,n):xs') = 
  if (t+i) `mod` n == 0 then run t (q*n) xs' else run (t+q) q xs