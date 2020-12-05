module DayZero where
import Data.List.Split

splitFile :: String -> String -> IO [String]
splitFile name splitter = do
        input <- readFile name
        let filtered = filter (\x -> x /= "") $ splitOn splitter input
        return filtered

splitAndReadFile :: Read a => String -> String -> IO [a]
splitAndReadFile name splitter = do
        input <- readFile name
        let filtered = filter (\x -> x /= "") $ splitOn splitter input
        return $ map read filtered