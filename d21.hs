{-# LANGUAGE ScopedTypeVariables #-}
import DayZero
import Data.List.Split
import Data.List
import qualified Data.Map as M

data Food = Food {
    ingredients :: [String],
    allergens :: [String]
} deriving (Show)


instance Read Food where
    readsPrec _ recipe = 
        let (ingredients:allergens:[]) = splitOn " (" recipe
        in [(Food (splitOn " " ingredients) (map (filter $ \x -> not $ x `elem` ",)") $ tail $ splitOn " " allergens), "")]

main :: IO ()
main = do
    input <- splitAndReadFile "input_d21.txt" "\n"
    let allergyMap =  run M.empty input
    let allIngredients = nub $ concat $ map ingredients input
    let potentialIngredients = nub $ concat $ (M.elems allergyMap)
    let safeIngredients = filter (\i -> not (i `elem` potentialIngredients)) allIngredients
    print $ length $ filter (`elem` safeIngredients) $ concat $ map ingredients input
    -- part 2
    let final = concat $ intersperse "," (map snd (sort $ run2 allergyMap))
    putStrLn final

run2 :: M.Map String [String] -> [(String, String)]
run2 a2f = run' a2f (M.toList a2f)
    where run' full ((allergen, foods):a2f) = 
           case foods of
            food:[] -> (allergen, food) : (run2 $ M.map (filter (/= food)) full)
            _ -> run' full a2f
          run' full [] = []
    

run :: M.Map String [String] -> [Food] -> M.Map String [String]
run a2f ((Food ingredients allergens):foods) = run (updateMap a2f ingredients allergens) foods
    where updateMap a2f ingredients (allergen:allergens) = 
            let updated = 
                 case M.lookup allergen a2f of 
                    Just (ingredients') -> M.insert allergen (ingredients `intersect` ingredients') a2f
                    Nothing -> M.insert allergen ingredients a2f
            in updateMap updated ingredients allergens
          updateMap a2f ingredients [] = a2f
run a2f _ = a2f