import DayZero
import Text.Read

data Expression = Node (Int -> Int -> Int) Expression Expression | Leaf Int

instance Show Expression where
    show (Node f e1 e2) = let op = if f 1 1 == 2 then "+" else "*" in "(" ++ op ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Leaf i) = show i


parse1 :: String -> Expression
parse1 input = parse' (filter (/= ' ') $ reverse input)
    where parse' str =
            case maybeSplit "*+" str of
                Just (str1, op, str2) -> Node (toFunc op) (parse' str1) (parse' str2)
                Nothing -> case str of
                    (')':rest) -> parse' $ reverse $ tail $ reverse rest
                    (i:[]) -> Leaf $ read [i]

parse2 :: String -> Expression
parse2 input = parse' (filter (/= ' ') $ reverse input)
    where parse' str = 
            case maybeSplit "*" str of
                Just (str1, op, str2) -> Node (toFunc op) (parse' str1) (parse' str2)
                Nothing -> case maybeSplit "+" str of
                    Just (str1, op, str2) -> Node (toFunc op) (parse' str1) (parse' str2)
                    Nothing -> case str of
                        (')':rest) -> parse' $ reverse $ tail $ reverse rest
                        (i:[]) -> Leaf $ read [i]


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
    input <- splitFile "input_d18.txt" "\n"
    let tot = sum $ map (evaluate . parse1) input
    print tot
    print $ sum $ map (evaluate . parse2) input


evaluate :: Expression -> Int
evaluate (Node op e1 e2) = op (evaluate e1) (evaluate e2)
evaluate (Leaf i) = i


toFunc :: Char -> Int -> Int -> Int
toFunc '+' = (+)
toFunc '*' = (*)