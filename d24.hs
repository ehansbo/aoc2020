import DayZero
import Data.Function.Memoize
import Debug.Trace

data Instructions = I [(Int, Int)] deriving (Show)
data Tile = Black | White deriving (Eq, Show)
type Floor = (Int, Int) -> Tile

instance Read Instructions where
    readsPrec _ input = [(I (read' input), "")]
        where read' [] = []
              read' (x:xs)
                | x == 's' || x == 'n' = getNS x (head xs) : read' (tail xs)
                | otherwise = getWE x : read' xs
              getNS f s = (if s == 'w' then (-1) else 1, if f == 'n' then (-1) else 1)
              getWE c = (if c == 'e' then (2, 0) else (-2, 0))

initFloor :: Floor
initFloor _ = White

main :: IO ()
main = do
    instructions <- splitAndReadFile "input_d24.txt" "\n" :: IO [Instructions]
    let floor = memoize $ foldl (\floor instructions -> execute floor instructions) initFloor instructions
    print $ countBlack floor
    let done = run 100 floor
    print $ countBlack done

run :: Int -> Floor -> Floor
run 0 f = f
run i f = run (i-1) (tick f)

tick :: Floor -> Floor
tick floor = memoize go
    where go (x, y)
            | floor (x, y) == Black = if length (filter (== Black) (getNeighbors floor (x, y))) `elem` [1,2] then Black else White
            | otherwise = if length (filter (== Black) (getNeighbors floor (x, y))) == 2 then Black else White

getNeighbors :: Floor -> (Int, Int) -> [Tile]
getNeighbors f (x, y) = map f [(x+2, y), (x+1, y+1), (x-1, y-1), (x-1, y+1), (x+1, y-1), (x-2, y)]

execute :: Floor -> Instructions -> Floor
execute f (I is) =
    let tile = foldl (\(x, y) (x', y') -> (x + x', y + y')) (0,0) is
    in  \tile' -> if tile == tile' then (swap (f tile')) else (f tile')

swap :: Tile -> Tile
swap White = Black
swap Black = White

countBlack :: Floor -> Int
countBlack floor = length $ filter (\tile -> floor tile == Black) [(x, y) | x <- [-200..200], y <- [-200..200]]