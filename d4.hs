import DayZero
import Data.List
import Data.List.Split
import System.IO.Unsafe

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]


main :: IO ()
main = do
    input <- splitFile "input_d4.txt" "\n\n"
    let valid = filter valid1 (map (replace '\n' ' ') input)
    putStrLn $ show $ length $ valid
    putStrLn $ show $ length $ filter valid2 valid

valid1 :: String -> Bool
valid1 str = foldl (&&) True (map (\a -> a `isInfixOf` str) requiredFields)

valid2 :: String -> Bool
valid2  = foldl (&&) True . map validateValues . splitOn " "

validateValues :: String -> Bool
validateValues ('b':'y':'r':':':v) = read v >= 1920 && read v <= 2002
validateValues ('i':'y':'r':':':v) = read v >= 2010 && read v <= 2020
validateValues ('e':'y':'r':':':v) = read v >= 2020 && read v <= 2030
validateValues ('h':'g':'t':':':v) = let (i, s):[] = reads v in if s == "cm" then i >= 150 && i <= 193 else if s == "in" then i >= 59 && i <= 76 else False
validateValues ('h':'c':'l':':':'#':v) = length v == 6 && length (filter (\a -> a `elem` "0123456789abcdef") v) == 6
validateValues ('e':'c':'l':':':v) = v `elem` eyeColors
validateValues ('p':'i':'d':':':v) = length v == 9 && length (filter (\a -> a `elem` "0123456789") v) == 9
validateValues ('c':'i':'d':':':_) = True
validateValues [] = True
validateValues _ = False


replace :: Char -> Char -> String -> String
replace c1 c2 (c:ss)
    | c1 == c = c2 : rest
    | otherwise = c : rest
        where rest = replace c1 c2 ss
replace _ _ [] = []