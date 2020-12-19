import Data.Function.Memoize
import Data.List.Split

data Rule = C Char | Seq [Rule] | Or Rule Rule | Unresolved Int deriving Show


main :: IO ()
main = do
    p1 <- run "input_d19.txt"
    p2 <- run "input_d19_p2.txt"
    putStrLn $ "part 1: " ++ show p1
    putStrLn $ "part 2: " ++ show p2

run :: String -> IO (Int)
run file = do
    input <- readFile file
    let (ri:ms:[]) = splitOn "\n\n" input
    let messages = splitOn "\n" ms
    let rules = memoize $ parseRules $ splitOn "\n" ri
    let rule0 = resolve rules 0
    return $ length $ filter (matches rule0) messages


matches :: Rule -> String -> Bool
matches rule str = 1 == (length $ filter (\(b, remainder) -> b && length remainder == 0) $ matchPartial rule str)

matchPartial :: Rule -> String -> [(Bool, String)]
matchPartial (C c) [] = [(False, [])]
matchPartial (C c) str = [(str !! 0 == c, tail str)]
matchPartial (Or r1 r2) str = matchPartial r1 str ++ matchPartial r2 str
matchPartial (Seq rules) str = matchSeq rules str

matchSeq :: [Rule] -> String -> [(Bool, String)]
matchSeq (rule:rules) str = let allMatches = filter ((== True) . fst) (matchPartial rule str) in
    concat (map (\(_, remainder) -> matchSeq rules remainder) allMatches)
matchSeq [] str = [(True, str)]

resolve :: (Int -> Rule) -> Int -> Rule
resolve rulesMap i = resolve' (rulesMap i)
    where resolve' (Or r1 r2) = Or (resolve' r1) (resolve' r2)
          resolve' (C c) = C c
          resolve' (Seq rs) = Seq (map (\r -> resolve' r) rs) 
          resolve' (Unresolved i) = resolve rulesMap i

parseRules :: [String] -> Int -> Rule
parseRules (unparsed:rules) x =
    let (no:rest:[]) = splitOn ": " unparsed in
        if x == read no then parseRule rest else parseRules rules x
parseRules [] x = error $ "cannot find rule " ++ show x

parseRule :: String -> Rule
parseRule str
    | '|' `elem` str = let (p1:p2:[]) = splitOn " | " str in Or (parseRule p1) (parseRule p2)
    | '"' `elem` str = C (str !! 1)
    | otherwise = Seq $ map (\x -> Unresolved $ read x) (splitOn " " str)
