import DayZero

type Grid = [[Bool]]

main :: IO ()
main = do
    rows <- splitFile "input_d3.txt" "\n"
    let grid = parseRows rows
    let base = countCollisions grid (0,0)
    let c1 = base (3,1)
    putStrLn $ show c1
    let mult = c1 * base (1, 1) * base (5, 1) * base (7, 1) * base (1, 2)
    putStrLn $ show mult

countCollisions :: Grid -> (Int, Int) -> (Int, Int) -> Int
countCollisions grid (x, y) (x', y')
    | y >= length grid = 0
    | isTree grid (x, y) = 1 + next
    | otherwise = next
        where next = countCollisions grid (x+x', y+y') (x', y')

parseRows :: [String] -> Grid
parseRows rows =  map (concat . repeat . parseRow) rows

parseRow :: String -> [Bool]
parseRow ('.':es) = False : parseRow es
parseRow ('#':es) = True : parseRow es
parseRow [] = []

isTree :: Grid -> (Int, Int) -> Bool
isTree grid (x, y) = (grid !! y) !! x