import DayZero

data Pwd = Pwd Int Int Char String
    deriving Show

instance Read Pwd where 
    readsPrec _ input = 
        let (i1, '-':s1):[] = reads input :: [(Int, String)]
            (i2, ' ':c:':':' ':pwd):[] = readsPrec 0 s1 :: [(Int, String)]
        in [(Pwd i1 i2 c pwd, "")]

main :: IO ()
main = do
    pwds <- splitAndReadFile "input_d2.txt" "\n"
    putStrLn $ show $ length $ filter valid1 pwds
    putStrLn $ show $ length $ filter valid2 pwds

valid1 :: Pwd -> Bool
valid1 (Pwd min max c pwd) = numChars <= max && numChars >= min
    where numChars = length $ filter (== c) pwd

-- boolean1 /= boolean2 is logically same as xor
valid2 :: Pwd -> Bool
valid2 (Pwd p1 p2 c pwd) = (pwd !! (p1 - 1) == c) /= (pwd !! (p2 - 1) == c)