import DayZero
import Text.Read

data Expression = Node (Int -> Int -> Int) Expression Expression | Leaf Int
data Expression2 = Node2 (Int -> Int -> Int) Expression2 Expression2 | Leaf2 Int

instance Show Expression where
    show (Node f e1 e2) = let op = if f 1 1 == 2 then "+" else "*" in "(" ++ op ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Leaf i) = show i

instance Show Expression2 where
    show (Node2 f e1 e2) = let op = if f 1 1 == 2 then "+" else "*" in "(" ++ op ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Leaf2 i) = show i

instance Read Expression where
    readsPrec _ input = [(parse (filter (/= ' ') $ reverse input), "")]
        where parse str = 
                case maybeSplit "*+" str of
                    Just (str1, op, str2) -> Node (toFunc op) (parse str1) (parse str2)
                    Nothing -> case str of
                        (')':rest) -> parse $ reverse $ tail $ reverse rest
                        (i:[]) -> Leaf $ read [i]

instance Read Expression2 where
    readsPrec _ input = [(parse (filter (/= ' ') $ reverse input), "")]
        where parse str = 
                case maybeSplit "*" str of
                    Just (str1, op, str2) -> Node2 (toFunc op) (parse str1) (parse str2)
                    Nothing -> case maybeSplit "+" str of
                        Just (str1, op, str2) -> Node2 (toFunc op) (parse str1) (parse str2)
                        Nothing -> case str of
                            (')':rest) -> parse $ reverse $ tail $ reverse rest
                            (i:[]) -> Leaf2 $ read [i]


maybeSplit :: String -> String -> Maybe (String, Char, String)
maybeSplit = maybeSplit' "" 0
    where maybeSplit' _ _ _ [] = Nothing
          maybeSplit' acc i splitters (c:s)
            | c `elem` splitters && i == 0 = Just (acc, c, s)
            | (c == ')') = maybeSplit' (acc ++ [c]) (i+1) splitters s
            | (c == '(') = maybeSplit' (acc ++ [c]) (i-1) splitters s
            | otherwise = maybeSplit' (acc ++ [c]) i splitters s

main :: IO ()
main = do
    input <- splitAndReadFile "input_d18.txt" "\n"
    let tot = sum $ map evaluate input
    print tot
    input2 <- splitAndReadFile "input_d18.txt" "\n"
    print $ sum $ map evaluate2 input2


evaluate :: Expression -> Int
evaluate (Node op e1 e2) = op (evaluate e1) (evaluate e2)
evaluate (Leaf i) = i

evaluate2 :: Expression2 -> Int
evaluate2 (Node2 op e1 e2) = op (evaluate2 e1) (evaluate2 e2)
evaluate2 (Leaf2 i) = i

toFunc :: Char -> Int -> Int -> Int
toFunc '+' = (+)
toFunc '*' = (*)