import Data.List.Split
import Data.List

main :: IO ()
main = do
    q <- map (splitOn "\n") . splitOn "\n\n" <$> readFile "input_d6.txt"
    print $ sum $ map (length . (foldl1 union)) q
    print $ sum $ map (length . (foldl1 intersect) . filter (/= "")) q