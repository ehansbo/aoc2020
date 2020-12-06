import DayZero
import Data.List.Split
import Data.List

main :: IO ()
main = do
    groups <- splitFile "input_d6.txt" "\n\n"
    let q = map (splitOn "\n") groups
    print $ sum $ map (length . (foldl1 union)) q
    print $ sum $ map (length . (foldl1 intersect) . filter (/= "")) q