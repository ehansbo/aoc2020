import DayZero

data Pass = P String String
    deriving (Show)

instance Read Pass where
    readsPrec _ input = [(P (take 7 input) (drop 7 input), "")]

main :: IO ()
main = do
    passes <- splitAndReadFile "input_d5.txt" "\n"
    let seated = map seatId passes
    print $ maximum $ seated
    let allIds = [0..127*8+7]
    let available = filter (\a -> not (a `elem` seated) && a + 1 `elem` seated && a - 1 `elem` seated) allIds
    print $ head available

seatId :: Pass -> Int
seatId p = 8 * row p + col p

row (P r _) = row' 64 r
    where row' mult ('F':ss) = row' (mult `div` 2) ss
          row' mult ('B':ss) = mult + row' (mult `div` 2) ss
          row' _ [] = 0


col (P _ c) = col' 4 c
    where col' mult ('L':ss) = col' (mult `div` 2) ss
          col' mult ('R':ss) = mult + col' (mult `div` 2) ss
          col' _ [] = 0