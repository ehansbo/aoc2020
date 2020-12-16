{-# LANGUAGE ScopedTypeVariables #-}

import DayZero
import Data.List.Split

data Rule = Rule String (Int -> Bool)
type Ticket = [Int]

instance Read Rule where
    readsPrec _ s = 
        let (str:rest:[]) = splitOn ":" s
            parts = words rest
            f x = splitOn "-" x
            min x = read (head x)
            max x = read (x !! 1)
            p1 = f $ parts !! 0
            p2 = f $ parts !! 2
            in [(Rule str (\x -> (x >= min p1 && x<= max p1) || (x >= min p2 && x <= max p2)), "")]


main :: IO ()
main = do
    -- parsing
    content <- splitFile "input_d16.txt" "\n"
    let (p1:p2:p3:[]) = splitWhen ((== ':') . last) content
    let (rules, myTicket, otherTickets) :: ([Rule], Ticket, [Ticket]) = (map read p1, map read $ splitOn "," (head p2), map (map read . splitOn ",") p3)
    -- part 1
    let invalidFields = map (filter (\x -> invalid x rules)) otherTickets
    print $ (sum . map sum) invalidFields
    -- part 2
    let validTickets = filter (\t -> length (filter (\x -> invalid x rules) t) == 0) otherTickets
    let possibleSlots = zip rules $ map (findPossibleSlots validTickets) rules
    let rulesAndSlots = map head $ map (\l -> filter (\x -> (length . snd) x == l) possibleSlots) [1..length possibleSlots]
    let departure = filter (\(Rule str _, _) -> take 3 str == "dep") $ getValidSlots rulesAndSlots
    print $ product $ map (\(_, pos) -> myTicket !! pos) departure

invalid :: Int -> [Rule] -> Bool
invalid x rules = not $ foldl (\b (Rule _ f) -> b || f x) False rules

findPossibleSlots :: [Ticket] -> Rule -> [Int]
findPossibleSlots tickets (Rule _ f) = findPossibleSlots' tickets [0..length (head tickets) - 1]
    where findPossibleSlots' (t:ts) currentPoss = findPossibleSlots' ts (filter (f . (t !!)) currentPoss)
          findPossibleSlots' [] c = c

getValidSlots :: [(Rule, [Int])] -> [(Rule, Int)]
getValidSlots = getValidSlots' []
    where getValidSlots' chosen ((rule, slots):rs) = (rule, slot):(getValidSlots' (slot:chosen) rs)
            where slot = head $ filter (\s -> not $ s `elem` chosen) slots
          getValidSlots' _ _ = []