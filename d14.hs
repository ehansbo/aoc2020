import DayZero
import Control.Monad.State.Strict
import System.IO.Unsafe
import qualified Data.Map as M
data Instruction = Mem Int Int | Mask String deriving (Show)
type Memory = M.Map Int Int

instance Read Instruction where
    readsPrec _ input = if take 4 input == "mask" 
        then parseMask $ (words input) !! 2 
        else parseMem $ words input
            where parseMask mask = [(Mask mask, "")]
                  parseMem memList = [(Mem ((fst . head) $ reads $ drop 4 $ head memList) (read $ memList !! 2), "")]

main :: IO ()
main = do
    input <- splitAndReadFile "input_d14.txt" "\n"
    let results = runF run input
    print $ sum $ map snd $ M.toList results
    let results2 = runF run2 input
    print $ sum $ map snd $ M.toList results2

runF :: (Instruction -> [Instruction] -> State Memory ()) -> [Instruction] -> Memory
runF f i = snd $ runState (f (head i) (tail i)) M.empty


run :: Instruction -> [Instruction] -> State Memory ()
run (Mask mask) (mask'@(Mask _):is) = run mask' is
run m@(Mask mask) ((Mem addr val):is) = do
    memory <- get
    let value = bitMap mask val
    put (M.insert addr value memory)
    run m is
run _ [] = return ()

run2 :: Instruction -> [Instruction] -> State Memory ()
run2 (Mask mask) (mask'@(Mask _):is) = run2 mask' is
run2 m@(Mask mask) ((Mem addr val):is) = do
    memory <- get
    let newAddrs = bitMap2 mask addr
    put (foldl (\m a -> M.insert a val m) memory newAddrs)
    run2 m is
run2 _ [] = return ()

bitMap2 :: String -> Int -> [Int] 
bitMap2 mask a = map b2d $ bitMap2' mask (d2b a)
  where bitMap2' ('X':ms) (_:vs) = m '0' ms vs ++ m '1' ms vs
        bitMap2' ('1':ms) (_:vs) = m '1' ms vs
        bitMap2' ('0':ms) (v:vs) = m v ms vs
        bitMap2' [] [] = [""]
        m c ms vs = map (\x -> c : x) (bitMap2' ms vs)

bitMap :: String -> Int -> Int
bitMap mask v = b2d $ bitMap' mask (d2b v)
    where bitMap' ('X':ms) (v:vs) = v : bitMap' ms vs
          bitMap' (m:ms) (_:vs) = m : bitMap' ms vs
          bitMap' [] [] = []

d2b :: Int -> String
d2b i = reverse $ take 36 $ d2b' i ++ ['0','0'..]
    where d2b' 0 = ""
          d2b' i = show (i `mod` 2) ++ d2b' (i `div` 2)

b2d :: String -> Int
b2d s = b2d' 1 $ reverse s
    where b2d' _ [] = 0
          b2d' i (x:xs) = i * (read [x]) + b2d' (i*2) xs