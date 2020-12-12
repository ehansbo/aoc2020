import DayZero

main :: IO ()
main = do
    input <- splitFile "input_d12.txt" "\n"
    let lastPosition = run (0, 0) (1, 0) input
    print $ distance lastPosition (0, 0)
    let lastPosition2 = run2 (0, 0) (10, -1) input
    print $ distance lastPosition2 (0, 0)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x2-x1) + abs (y2-y1)

run :: (Int, Int) -> (Int, Int) -> [String] -> (Int, Int)
run pos _ [] = pos
run (x, y) (dx, dy) (z:zs) = 
    let cp (cx, cy) i = run (x + cx * read i, y + cy * read i) (dx, dy) zs 
        cd dir i = run (x, y) (changeDirection (dx, dy) dir i) zs in
    case z of
        'F':i -> cp (dx, dy) i
        'N':i -> cp (0, -1) i
        'E':i -> cp (1, 0) i
        'W':i -> cp (-1, 0) i
        'S':i -> cp (0, 1) i
        'L':i -> cd (-1) (read i)
        'R':i -> cd 1 (read i)

run2 :: (Int, Int) -> (Int, Int) -> [String] -> (Int, Int)
run2 pos _ [] = pos
run2 (x, y) (dx, dy) (z:zs) = 
    let cp (cx, cy) i = run2 (x, y) (dx + cx * read i, dy + cy * read i) zs 
        cd dir i = run2 (x, y) (changeDirection (dx, dy) dir i) zs
        forward i = run2 (x+i*dx, y+i*dy) (dx, dy) zs in do
    case z of
        'F':i -> forward (read i)
        'N':i -> cp (0, -1) i
        'E':i -> cp (1, 0) i
        'W':i -> cp (-1, 0) i
        'S':i -> cp (0, 1) i
        'L':i -> cd (-1) (read i)
        'R':i -> cd 1 (read i)


changeDirection :: (Int, Int) -> Int -> Int -> (Int, Int)
changeDirection (dx, dy) dir degrees 
    | degrees == 90 = (-dy*dir, dx*dir)
    | degrees == 180 = (-dx, -dy)
    | degrees == 270 = (dy*dir, -dx*dir)