import DayZero
import Data.List.Split
import Data.List
import Data.Function.Memoize
import Debug.Trace

data Block = Dot | Hash | Outside deriving (Eq)
data Grid = Grid {
    index :: Int,
    func :: ((Int, Int) -> Block)
}

data Dir = U | L | R | D

size :: Grid -> Int
size (Grid i f) = size' f 

-- is always a square. Name is slightly misleading, actually returns the max x (and max y) 
size' :: ((Int, Int) -> Block) -> Int
size' f = length (takeWhile (\x -> f (x, 0) /= Outside) [0..]) - 1 

instance Eq Grid where
    (==) g1 g2 = index g1 == index g2

instance Show Block where
    show Dot = "."
    show Hash = "#"

instance Read Grid where
    readsPrec _ str = 
        let (topRow:gridStr:[]) = splitOn (":\n") str
            gridList = (splitOn "\n" gridStr)
            toBlock '#' = Hash
            toBlock '.' = Dot
            f (x, y) = if (x >= 10 || y >= 10 || x < 0 || y < 0) then Outside else toBlock $ (gridList !! y) !! x in
        [(Grid (read $ drop 5 topRow) (memoize f), "")]

instance Show Grid where
    show (Grid i f) = show i ++ "\n" ++ unlines [[(head . show) (f (x, y)) | x <- [0..size' f]] | y <- [0..size' f]]

seaMonsterCoords = [(0, 1), (1, 2), (4, 2), (5, 1), (6, 1), (7, 2), (10, 2), (11, 1), (12, 1), (13, 2), (16, 2), (17, 1), (18, 1), (18, 0), (19, 1)]

main :: IO ()
main = do
    --part 1
    input <- splitAndReadFile "input_d20.txt" "\n\n"
    let neighbors = map (\g -> (g, getNeighbors input g)) input
    let corners = filter ((== 2) . length . snd) neighbors
    print $ product $ map (index . fst) corners
    --part 2
    let gridOfGrids = getFullGrid (head corners) neighbors
    let bigGrid = getBigGrid gridOfGrids
    let seaMonsterPotentials = map findSeaMonsters (allChoicesGeneric bigGrid)
    let numberOfHashes = length $ filter (== '#') (show bigGrid)
    let maxSeaMonsters = head $ filter (/= 0) seaMonsterPotentials
    print $ numberOfHashes - maxSeaMonsters*(length seaMonsterCoords)


findSeaMonsters :: Grid -> Int
findSeaMonsters grid = length $ filter (\(x, y) -> seaMonsterAt (x, y) grid) [(x, y) | x<-[0..size grid], y <- [0..size grid]]

seaMonsterAt :: (Int, Int) -> Grid -> Bool
seaMonsterAt (x, y) (Grid _ f) =
    let h (a, b) = f (x+a, y+b) == Hash
    in and $ map h seaMonsterCoords

getBigGrid :: [[Grid]] -> Grid
getBigGrid matrix = Grid 1337 (\(x, y) -> 
    let gx = x `div` 8
        gy = y `div` 8
        x' = x `mod` 8
        y' = y `mod` 8
    in if gx < 0 || gy < 0 || gx >= length matrix || gy >= length matrix then Outside else func ((matrix !! gy) !! gx) (x'+1, y'+1))

getFullGrid :: (Grid, [Grid]) -> [(Grid, [Grid])] -> [[Grid]]
getFullGrid (topRCorner, neighbors) other = 
    let right = head neighbors
        down = neighbors !! 1
        aligned = alignFirst topRCorner right down
    in (aligned : getFullRow (alignTo right aligned) other) : (getFullGrid' (alignTo down aligned) other)


getFullRow :: Grid -> [(Grid, [Grid])] -> [Grid]
getFullRow aligned other = 
    let neighbors = get other aligned
        rightEdge = getEdge R aligned
        neighborToRight = filter (\grid -> rightEdge `elem` concat (map allEdges (allChoices grid))) neighbors
    in if length neighborToRight /= 1 then [aligned] else aligned : getFullRow (alignTo (head neighborToRight) aligned) other 

getFullGrid' :: Grid -> [(Grid, [Grid])] -> [[Grid]]
getFullGrid' aligned other =
    let neighbors = get other aligned
        row = getFullRow aligned other 
        bottomEdge = getEdge D aligned
        neighborsDown = filter (\grid -> bottomEdge `elem` concat (map allEdges (allChoices grid))) neighbors
    in if length neighborsDown /= 1 then [row] else row : getFullGrid' (alignTo (head neighborsDown) aligned) other

get :: [(Grid, [Grid])] -> Grid -> [Grid]
get ((g',ns):gs) g
    | g' == g = ns
    | otherwise = get gs g

alignTo :: Grid -> Grid -> Grid
alignTo toAlign target = 
    let possibleAlignments = allChoices toAlign
    in  head $ filter (\alignment -> areAligned alignment target) possibleAlignments

areAligned :: Grid -> Grid -> Bool
areAligned g1 g2 = getEdge U g1 == getEdge D g2 || getEdge L g1 == getEdge R g2 || getEdge R g1 == getEdge L g2 || getEdge D g1 == getEdge U g2

-- aligns a grid to have one neighbor to the right and another neighbor down.
alignFirst :: Grid -> Grid -> Grid -> Grid
alignFirst toAlign targetR targetD = 
    let possibleTargetsR = map (getEdge L) (allChoices targetR)
        possibleTargetsD = map (getEdge U) (allChoices targetD)
    in head $ filter (\x -> (getEdge R x) `elem` possibleTargetsR && (getEdge D x) `elem` possibleTargetsD) (allChoices toAlign)



getNeighbors :: [Grid] -> Grid -> [Grid]
getNeighbors grids grid = filter (\g -> grid /= g && isNeighbor g grid) grids

flipGrid :: Int -> Grid -> Grid
flipGrid max (Grid i f) = Grid i (\(x, y) -> f (max - x, y))

rotateGrid :: Int -> Grid -> Grid
rotateGrid max (Grid i f) = Grid i (\(x, y) -> f (y, max - x))

allChoices :: Grid -> [Grid]
allChoices g = 
    let r = rotateGrid 9
        fg = flipGrid 9 g
    in [g, r g, (r . r) g, (r . r . r) g, fg, r fg, (r . r) fg, (r . r . r) fg]

allChoicesGeneric :: Grid -> [Grid]
allChoicesGeneric g = 
    let r = rotateGrid (size g)
        fg = flipGrid (size g) g
    in [g, r g, (r . r) g, (r . r . r) g, fg, r fg, (r . r) fg, (r . r . r) fg]


isNeighbor :: Grid -> Grid -> Bool
isNeighbor g1 g2 = length (potentialURows (allChoices g1) `intersect` potentialURows (allChoices g2)) > 0
    where potentialURows = map (\g -> [(func g) (x, 0) | x <- [0..9]])

getEdge :: Dir -> Grid -> [Block]
getEdge U (Grid _ f) = [f (x, 0) | x <- [0..9]]
getEdge L (Grid _ f) = [f (0, y) | y <- [0..9]]
getEdge R (Grid _ f) = [f (9, y) | y <- [0..9]]
getEdge D (Grid _ f) = [f (x, 9) | x <- [0..9]]

allEdges :: Grid -> [[Block]]
allEdges grid = map (\f -> f grid) [getEdge U, getEdge L, getEdge R, getEdge D]

