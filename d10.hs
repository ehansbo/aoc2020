import DayZero
import Data.List
import Data.MemoTrie

main :: IO ()
main = do
    input <- splitAndReadFile "input_d10.txt" "\n"
    let sorted = 0 : sort input ++ [maximum input + 3]
    print $ findDifference 1 sorted * findDifference 3 sorted
    print $ findMemo sorted

findDifference :: Int -> [Int] -> Int
findDifference i (x:y:xs)
    | y - x == i = 1 + findDifference i (y:xs)
    | otherwise = findDifference i (y:xs)
findDifference _ _ = 0

findMemo :: [Int] -> Int
findMemo = memo findNumberOfArrangements

findNumberOfArrangements :: [Int] -> Int
findNumberOfArrangements (x:[]) = 1
findNumberOfArrangements (x:xs) = findMemo (dropper 3) + findMemo (dropper 2) + findMemo (dropper 1)
    where dropper i = dropWhile (\y -> y - x /= i) xs
findNumberOfArrangements [] = 0
