import qualified Data.IntMap as M

main :: IO ()
main = do
    let input = [1,2,16,19,18,0]
    let output = input ++ run (reverse input)
    print $ output !! (2020 - 1)
    let output2 = run2 30000000 (length input) (last input) (M.fromList $ zip input [1..])
    print output2

run :: [Int] -> [Int]
run all@(latest:xs) = let next i = i : run (i : all) in
    if latest `elem` xs 
        then next $ 1 + (length $ takeWhile (/= latest) xs)
        else next 0

run2 :: Int -> Int -> Int -> M.IntMap Int -> Int
run2 end i prev map
    | end == i = prev
    | not (prev `M.member` map) = run2 end (i+1) 0 (M.insert prev i map)
    | otherwise = run2 end (i+1) v (M.insert prev i map)
        where v = i - lookup
              (Just lookup) = M.lookup prev map