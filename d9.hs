import DayZero
import Data.List

main :: IO ()
main = do
    input <- splitAndReadFile "input_d9.txt" "\n"
    let (pre, rest) = splitAt 25 input
    let p1 = head $ nonSummable pre rest 
    print p1
    let p2 = head $ findContiguous p1 input
    print $ maximum p2 + minimum p2

nonSummable :: [Int] -> [Int] -> [Int]
nonSummable pre (x:xs) = if containsPair x pre then rest else x : rest
    where rest = nonSummable (drop 1 pre ++ [x]) xs
nonSummable _ [] = []


containsPair :: Int -> [Int] -> Bool
containsPair x (p:ps) = length (filter (== x-p) ps) > 0 || containsPair x ps
containsPair _ [] = False

findContiguous :: Int -> [Int] -> [[Int]]
findContiguous i xs = filter ((== i) . sum) $ subLists $ filter (< i) xs

subLists :: [a] -> [[a]]
subLists [] = [[]]
subLists (x:xs) = subLists xs ++ [x:subList | subList <- inits xs]