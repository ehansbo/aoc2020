import DayZero
import qualified Data.Set as S

data Program = P { pos :: Int, acc :: Int, instr :: [Instruction] }
data Instruction = Jmp Int | Nop Int | Acc Int deriving (Show)

instance Read Instruction where
    readsPrec _ input
        | instr == "jmp" = [(Jmp i, "")]
        | instr == "nop" = [(Nop i, "")]
        | instr == "acc" = [(Acc i, "")]
        | otherwise = []
            where instr = head $ words input
                  i = read $ if '+' `elem` input then drop 5 input else drop 4 input

main :: IO ()
main = do
    input <- splitAndReadFile "input_d8.txt" "\n"
    print $ programTerminates (initialize input)
    print $ head $ dropWhile (not . snd) $ map (programTerminates . initialize) $ replaceInstr input

replaceInstr :: [Instruction] -> [[Instruction]]
replaceInstr instr = replaceInstr' 0
    where replaceInstr' i = case instr !! i of
            Nop n -> insert i (Jmp n) : replaceInstr' (i+1)
            Jmp n -> insert i (Nop n) : replaceInstr' (i+1)
            _ -> replaceInstr' (i+1)
          insert i inst = take i instr ++ [inst] ++ drop (i+1) instr


programTerminates :: Program -> (Int, Bool)
programTerminates = programTerminates' S.empty
    where programTerminates' visited program 
            | (pos next) == (length $ instr program) = (acc program, True)
            | (pos next) > (length $ instr program) = (acc program, False)
            | (pos next) `S.member` visited = (acc program, False)
            | otherwise = programTerminates' (S.insert (pos next) visited) next
                where next = run program

run :: Program -> Program
run (P pos acc instr) = P pos' acc' instr
    where (pos', acc') = runInstr (instr !! pos) (pos, acc)

runInstr :: Instruction -> (Int, Int) -> (Int, Int)
runInstr (Jmp i) (pos, acc) = (pos + i, acc)
runInstr (Nop _) (pos, acc) = (pos + 1, acc)
runInstr (Acc i) (pos, acc) = (pos + 1, acc + i)

initialize :: [Instruction] -> Program
initialize = P 0 0