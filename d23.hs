import Data.List.Split
import Debug.Trace
import qualified Data.Map as M

main :: IO ()
main = do
    let testInput = [3,8,9,1,2,5,4,6,7]
    let input = [7,8,4,2,3,5,9,1,6]
    let result = makeMoves (maximum input) (head input) 100 (getMap input)
    
    putStrLn $ concat $ map show ((tail .take 9 . backToList 1) result)

    let input2 = input ++ [10,11..1000000]
    let result2 = makeMoves (last input2) (head input2) 10000000 (getMap input2)
    let final = backToList 1 result2
    print $ (final !! 1) * (final !! 2)
    

backToList :: Int -> M.Map Int Int -> [Int]
backToList curr currMap = let (Just next) = M.lookup curr currMap in curr:backToList next currMap

makeMoves :: Int -> Int -> Int -> M.Map Int Int -> M.Map Int Int
makeMoves _ _ 0 currMap = currMap
makeMoves max curr i currMap = let (newCurr, newMap) = makeMove max curr currMap in (makeMoves max newCurr (i-1) newMap)

makeMove :: Int -> Int -> M.Map Int Int -> (Int, M.Map Int Int)
makeMove max curr currMap =
    let (Just x) = M.lookup curr currMap
        (Just y) = M.lookup x currMap
        (Just z) = M.lookup y currMap
        (Just afterZ) = M.lookup z currMap
        destf 0 = destf max
        destf i = if i `elem` [x,y,z] then destf $! (i-1) else i
        dest = destf (curr-1)
        (Just afterDest) = M.lookup dest currMap
    in (afterZ, (M.insert dest x . M.insert curr afterZ . M.insert z afterDest) currMap)


getMap :: [Int] -> M.Map Int Int
getMap (x:xs) = getMap' x (x:xs) M.empty
    where getMap' first (x:y:xs) currentMap = getMap' first (y:xs) (M.insert x y currentMap)
          getMap' first (x:[]) currentMap = M.insert x first currentMap

score :: [Int] -> (Int, Int)
score (x:1:z:_) = (x, z)
score (x:xs) = score xs 

