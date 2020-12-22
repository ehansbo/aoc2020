import DayZero
import Data.List.Split
import qualified Data.Set as S

type Deck = [Int]
data Player = P1 | P2 deriving (Eq)

main :: IO ()
main = do
    (p1:p2:[]) <- splitFile "input_d22.txt" "\n\n"
    let f p = (map read) $ tail $ filter (/= "") (splitOn "\n" p) :: [Int]
    let (deck1, deck2) = (f p1, f p2)
    let winningDeck = play deck1 deck2
    print $ score winningDeck
    let (winningDeck2, _) = play2 deck1 deck2
    print $ score winningDeck2

play2 :: Deck -> Deck -> (Deck, Player)
play2 = play2' S.empty S.empty
    where play2' prev1 prev2 d1@(x:xs) d2@(y:ys)
            | (hash d1) `elem` prev1 && (hash d2) `elem` prev2 = (d1, P1)
            | otherwise = if winner == P1 then play2' np1 np2 (xs ++ [x, y]) ys else play2' np1 np2 xs (ys ++ [y, x])
                where winner = if x > length xs || y > length ys 
                               then if x > y then P1 else P2
                               else snd (play2 (take x xs) (take y ys)) 
                      np1 = S.insert (hash d1) prev1
                      np2 = S.insert (hash d2) prev2
          play2' _ _ [] ys = (ys, P2)
          play2' _ _ xs [] = (xs, P1)

play :: Deck -> Deck -> Deck
play (x:xs) (y:ys)
    | x > y = play (xs ++ [x,y]) ys
    | y > x = play xs (ys ++ [y,x])
play [] ys = ys
play xs [] = xs

score :: Deck -> Int
score deck = sum $ map (\(x, y) -> x * y) (zip [1..] (reverse deck))

hash :: Deck -> Int 
hash deck = sum $ map (\(x, y) -> x * y) (zip (map (\(x, y) -> x ^ y) (zip [100,100..] [0..])) deck)