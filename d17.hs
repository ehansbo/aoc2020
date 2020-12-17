{-# LANGUAGE TypeOperators,TypeFamilies #-}

import DayZero
import Data.MemoTrie
import Control.Arrow (first)

type Grid  = (Int, Int, Int) -> Bool
type Grid2 = (Int, Int, Int, Int) -> Bool

instance (HasTrie a, HasTrie b, HasTrie c, HasTrie d) => HasTrie (a,b,c,d) where
  newtype (a,b,c,d) :->: x = QuadrupleTrie (((a,b),(c,d)) :->: x)
  trie f = QuadrupleTrie (trie (f . quad))
  untrie (QuadrupleTrie t) = untrie t . dequad
  enumerate (QuadrupleTrie t) = enum' quad t

quad :: ((a,b),(c,d)) -> (a,b,c,d)
quad ((a,b),(c,d)) = (a,b,c,d)

dequad :: (a,b,c,d) -> ((a,b),(c,d))
dequad (a,b,c,d) = ((a,b),(c,d))

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap.first) f . enumerate

main :: IO ()
main = do
    input <- splitFile "input_d17.txt" "\n"
    let ticks = 6
    let grid = parseGrid input
    print $ countGrid (tickMultiple grid ticks) ticks (length input)
    let grid2 = parseGrid2 input
    print $ countGrid2 (tickMultiple2 grid2 ticks 1 (length input)) ticks (length input)



tickMultiple :: Grid -> Int -> Grid
tickMultiple grid 0 = grid
tickMultiple grid i = tickMultiple (tick grid) (i-1)

tickMultiple2 :: Grid2 -> Int -> Int -> Int -> Grid2
tickMultiple2 grid 0 _ _ = grid
tickMultiple2 grid i tickNo max = tickMultiple2 (tick2 grid tickNo max) (i-1) (tickNo + 1) max

tick :: Grid -> Grid
tick grid = memo go
    where go (x, y, z) = 
            if grid (x, y, z) 
                then neigh == 3 || neigh == 2
                else neigh == 3
            where neigh = length (filter grid (neighbours (x, y, z)))


tick2 :: Grid2 -> Int -> Int -> Grid2
tick2 grid tickNo max = memo go
    where go (x, y, z, w) = 
            if x < -tickNo || y < -tickNo || z < -tickNo || w < -tickNo || x > tickNo + max || y > tickNo + max || z > tickNo || w > tickNo then False else
                if grid (x, y, z, w) 
                    then neigh == 3 || neigh == 2
                    else neigh == 3
            where neigh = length (filter (\(x', y', z', w') -> x' >= -tickNo && y' >= -tickNo && z' >= -tickNo && w' >= -tickNo && x' <= tickNo + max && y' <= tickNo + max && z' <= tickNo && w' <= tickNo && grid (x', y', z', w')) (neighbours2 (x, y, z, w)))

neighbours :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbours (x, y, z) = [(x', y', z') | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], (x, y, z) /= (x', y', z')]

neighbours2 :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
neighbours2 (x, y, z, w) = [(x', y', z', w') | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], w' <- [w-1..w+1], (x, y, z, w) /= (x', y', z', w')]

parseGrid :: [String] -> Grid
parseGrid ss (x, y, 0) = y < length ss && y >= 0 && x < length (head ss) && x >= 0 && (ss !! y) !! x == '#'
parseGrid ss _ = False

parseGrid2 :: [String] -> Grid2
parseGrid2 ss (x, y, 0, 0) = y < length ss && y >= 0 && x < length (head ss) && x >= 0 && (ss !! y) !! x == '#'
parseGrid2 ss _ = False


countGrid :: Grid -> Int -> Int -> Int
countGrid grid iter max = length [(x, y, z) | x <- [-iter..iter+max], y <- [-iter..iter+max], z <- [-iter..iter], grid (x, y, z)]

countGrid2 :: Grid2 -> Int -> Int -> Int
countGrid2 grid iter max = length [(x, y, z, w) | x <- [-iter..iter+max], y <- [-iter..iter+max], z <- [-iter..iter], w <- [-iter..iter], grid (x, y, z, w)]