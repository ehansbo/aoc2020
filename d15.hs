import qualified Data.Map as M


main :: IO ()
main = do
    let inputTest = [0,3,6]
    let input = [1,2,16,19,18,0]
    let output = input ++ run (reverse input)
    print $ output !! (2020 - 1)
    output2 <- run2 30000000 (length input) (last input) (initializeMap input)
    print output2



run :: [Int] -> [Int]
run all@(latest:xs)
    | latest `elem` xs = index : run (index : all)
    | otherwise = 0 : run (0 : all)
        where index = 1 + (length $ takeWhile (/= latest) xs)

run2 :: Int -> Int -> Int -> M.Map Int Int -> Int
run2 end i prev map
    | end == i = prev
    | not (prev `M.member` map) = run2 end (i+1) 0 (M.insert prev i map)
    | otherwise = run2 end (i+1) v (M.insert prev i map)
        where v = i - lookup
            (Just lookup) = M.lookup prev map

initializeMap :: [Int] -> M.Map Int Int
initializeMap = initializeMap' 1 M.empty
    where initializeMap' i map (x:xs) = initializeMap' (i+1) (M.insert x i map) xs
          initializeMap' _ map [] = map