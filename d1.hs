import DayZero

main :: IO ()
main = do
    nums <- fileIntegers "input_d1.txt" "\n"
    let Just (n1, n2) = getSumPair 2020 nums
    putStrLn $ show $ n1 * n2
    let (t1, t2, t3) = getSumTriples 2020 nums
    putStrLn $ show $ t1 * t2 * t3

getSumPair :: Int -> [Int] -> Maybe (Int, Int)
getSumPair sum (n1:ns) =
    case filter ((==) (sum - n1)) ns of 
        (n2:_) -> Just (n1, n2)
        [] -> getSumPair sum ns
getSumPair _ [] = Nothing

getSumTriples :: Int -> [Int] -> (Int, Int, Int)
getSumTriples sum (n1:ns) = 
    case getSumPair (sum - n1) ns of
        Just (n2, n3) -> (n1, n2, n3)
        Nothing -> getSumTriples sum ns