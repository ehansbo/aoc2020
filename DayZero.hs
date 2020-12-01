module DayZero where
import Data.List.Split

fileIntegers :: String -> String -> IO [Int]
fileIntegers name splitter = do
        input <- readFile name
        let filtered = filter (\x -> x /= "") $ splitOn splitter input
        let lines = map read filtered :: [Int]
        return lines