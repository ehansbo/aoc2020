import DayZero
import Data.List
import Data.List.Split
import qualified Data.Set as Set

data Bag = Bag String deriving (Show, Eq, Ord)
data Rule = Rule Bag [Contains] deriving (Show)
data Contains = Contains { getNum :: Int, getBag :: Bag } deriving (Show)

instance Read Bag where
    readsPrec _ input =
        let s1:s2:('b':'a':'g':_):ss = words input
        in [(Bag (s1 ++ " " ++ s2), intercalate " " ss)]

instance Read Contains where
    readsPrec _ input =
        let [(num, ' ':b)] = reads input
            bag = read b
        in [(Contains num bag, "")]

instance Read Rule where
    readsPrec _ input = 
        let [(bag, ss)] = reads input
            "contain":ws = words ss
            contains = if head ws == "no" then [] else map read $ splitOn "," $ intercalate " " ws
        in [(Rule bag contains, "")]


main :: IO ()
main = do
    input <- splitAndReadFile "input_d7.txt" "\n"
    let cs = carriers input (Bag "shiny gold")
    print $ Set.size $ Set.fromList cs
    print $ bagsContained (Bag "shiny gold") input

carriers:: [Rule] -> Bag -> [Bag]
carriers rules bag = do
    (Rule container contains) <- rules
    if bag `partOf` contains then container:(carriers rules container) else [] 

bagsContained :: Bag -> [Rule] -> Int
bagsContained b rules = bagsContained' b rules rules
    where bagsContained' bag ((Rule bag' contains):rules) allRules
            | bag == bag' = sum (map (\c -> getNum c * (1 + bagsContained (getBag c) allRules)) contains)
            | otherwise   = bagsContained' bag rules allRules

partOf :: Bag -> [Contains] -> Bool
partOf bag ((Contains i bag'):cs) = bag == bag' || bag `partOf` cs
partOf bag _ = False