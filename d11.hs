import Data.List.Split
import Data.MemoTrie

data Seat = Empty | Occupied | Floor | Outside deriving (Eq)
type Point = (Int, Int)
type Grid = Point -> Seat

instance Show Seat where
    show Empty = "L"
    show Floor = "."
    show Occupied = "#"

instance Read Seat where
    readsPrec _ "L" = [(Empty, "")]
    readsPrec _ "." = [(Floor, "")]
    readsPrec _ "#" = [(Occupied, "")]

main :: IO ()
main = do
    file <- readFile "input_d11.txt"
    let input = map (map read . chunksOf 1) $ filter (\x -> x /= "") $ splitOn "\n" file
    let grid = memo $ initialize input
    let stable1 = findStable (tick n1 4) grid
    let stable2 = findStable (tick n2 5) grid
    putStrLn $ showGrid stable1
    putStrLn $ "\n" ++ showGrid stable2
    print $ count Occupied stable1
    print $ count Occupied stable2

count :: Seat -> Grid -> Int
count typ grid = length $ filter (== typ) $ map grid $ allPoints grid

initialize :: [[Seat]] -> Grid
initialize seats (x, y)
    | x < 0 || y < 0 = Outside
    | y >= length seats || x >= length (head seats) = Outside
    | otherwise = seats !! y !! x
    
showGrid :: Grid -> String
showGrid grid = showGrid' $ allPoints grid
    where showGrid' ((x,y):xs) = filter (\_ -> x == 0 && y > 0) "\n" ++ show (grid (x, y)) ++ showGrid' xs
          showGrid' [] = ""

findStable :: (Grid -> Grid) -> Grid -> Grid
findStable f grid
    | equals grid' grid = grid
    | otherwise = findStable f grid'
        where grid' = f grid

tick :: (Grid -> Point -> [Seat]) -> Int -> Grid -> Grid
tick neighbors o2e grid = memo go
    where go (x, y)
            | grid (x, y) == Empty && length (filter (== Occupied) $ neighbors grid (x, y)) == 0 = Occupied
            | grid (x, y) == Occupied && length (filter (== Occupied) $ neighbors grid (x, y)) >= o2e = Empty
            | otherwise = grid (x, y)

n1 :: Grid -> Point -> [Seat]
n1 grid (x, y) = [grid (x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], not (x' == x && y' == y)]  

n2 :: Grid -> Point -> [Seat]
n2 grid (x, y) = map dir [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]
    where dir (dx, dy) = if dropped == [] then Empty else head dropped
            where dropped = dropWhile (== Floor) $ takeWhile (/= Outside) $ map grid $ zip [x+dx,x+2*dx..] [y+dy, y+2*dy..]


equals :: Grid -> Grid -> Bool
equals g1 g2 = foldl (\b coord -> b && g1 coord == g2 coord) True $ allPoints g1

allPoints :: Grid -> [Point]
allPoints grid = [(x, y) | y <- takeWhile (\y -> grid (0, y) /= Outside) [0..], x <- takeWhile (\x -> grid (x, 0) /= Outside) [0..]]