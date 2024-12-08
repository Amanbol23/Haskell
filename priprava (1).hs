
printMaze :: Maze -> IO ()
printMaze x = putStr (concat (map (++"\n") x))

sample1dot :: Maze
sample1dot = [
           "*********",
           "*.*...*.*",
           "*.*.*.*.*",
           "*.*.*.*.*",
           "*...*...*",
           "*******.*",
           "........*",
           "*********"]

sample1space :: Maze
sample1space = ["*********",
                "* *   * *",
                "* * * * *",
                "* * * * *",
                "*   *   *",
                "******* *",
                "        *",
                "*********"]

sample2 :: Maze
sample2 = ["       ",     
           "       ",
           "  ***  ",
           "  ***  ",
           "  ***  ",
           "       ",
           "       "]

ms :: Maze
ms = ["*.......",
           ".....*..",
           "..*.*#..",
           "..*.*#..",
           "..***#..",
           ".#####..",
           "......*.",
           "........"]

putIntoMaze :: Maze -> [(Int, Int, Char)] -> Maze
putIntoMaze maze [] = maze
putIntoMaze maze ((a, b, c) : xs) =
  let rows_before = take a maze
      row_current = head (drop (length rows_before) maze)
      char_before = take b row_current
      char_after = drop (length char_before +1) row_current
      row_after = drop (length rows_before +1) maze
      in putIntoMaze (rows_before ++ [char_before ++ [c] ++ char_after] ++ row_after) xs

solveNum :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
solveNum maze (x, y) end seen delka =
        let above | x-1 >=0 = (maze !! (x-1)) !! y
                  | otherwise = '*'
            below | x+1 < length maze = (maze !! (x+1)) !! y
                  | otherwise = '*'
            left  | y-1 >= 0 = (maze !! x) !! (y-1)
                  | otherwise = '*'
            right | y+1 < length (maze !! x) = (maze !! x) !! (y+1)
                  | otherwise = '*'
            test_above  | ((x-1), y) == end = end : (x,y) : seen
                        | above == ' ' && ((x-1), y) `notElem` seen = solveNum maze ((x-1), y) end ((x,y):seen) (delka+1)
                        | otherwise = []
            test_below  | ((x+1), y) == end = end : (x,y) : seen
                        | below == ' ' && ((x+1), y) `notElem` seen = solveNum maze ((x+1), y) end ((x,y):seen) (delka+1)
                        | otherwise = []
            test_left   | (x, (y-1)) == end = end : (x,y) : seen
                        | left == ' ' && (x, (y-1)) `notElem` seen = solveNum maze (x, (y-1)) end ((x,y):seen) (delka+1)
                        | otherwise = []
            test_right  | (x, (y+1)) == end = end : (x,y) : seen
                        | right == ' ' && (x, (y+1)) `notElem` seen = solveNum maze (x, (y+1)) end ((x,y):seen) (delka+1)
                        | otherwise = []
            in (if (delka > (length maze * length (maze !! 0))) || all null [test_above, test_below, test_left, test_right] then [] else minimumBy (compare `on` length) ([ n | n <- [test_above, test_below, test_left, test_right], not $ null n]))


--Z2
-- Varianta, kde vykresli cestu vyhleda a neridi se stringem
makePath :: Maze -> (Int, Int) -> (Int, Int) -> Maze
makePath maze start end =
      let path = reverse (solveNum maze start end [] 0)
            in putIntoMaze maze [(fst (path !! i), snd (path !! i), head $ show (i `mod` 10)) | i <- [0..length(path)-1]]


--Z1
markPath :: Maze -> (Int, Int) -> String -> Maze
markPath maze start directions = putIntoMaze maze (setCharDir start directions (head directions) True)

-- [visited] -> current_direction (0 - first, 1-4 - up, right, down left) -> [putIntoMaze_format]
setChar :: [(Int, Int)] -> Int -> [(Int, Int, Char)]
setChar [(x,y)] _ = [(x, y, '+')]
setChar ((x, y) : (x2, y2) : xs) dir =
  let new_dir
        | y < y2 = 1
        | y > y2 = 3
        | x < x2 = 2
        | x > x2 = 4
    in  if dir == 0 then (x, y, '+') : setChar ((x2, y2):xs) new_dir
        else
          if x /= x2 then  -- horizontal movement
            if even dir then -- same dir
              (x, y, '|') : setChar ((x2, y2):xs) new_dir
            else -- different dir
              (x, y, '+') : setChar ((x2, y2):xs) new_dir
          else -- vertical movement
            if odd dir then -- same dir
              (x, y, '-') : setChar ((x2, y2):xs) new_dir
            else
              (x, y, '+') : setChar ((x2, y2):xs) new_dir


setCharDir :: (Int, Int) -> String -> Char -> Bool -> [(Int, Int, Char)]
setCharDir (dir_y, dir_x) [_] _  _= [(dir_y, dir_x, '+')]
setCharDir (dir_y, dir_x) (x:xs) prev first
  | x == 'd' = if prev == 'd' 
    then 
      if first then (dir_y, dir_x, '+') : setCharDir (dir_y+1, dir_x) xs x False
      else (dir_y, dir_x, '|') :          setCharDir (dir_y+1, dir_x) xs x False
    else (dir_y, dir_x, '+') :            setCharDir (dir_y+1, dir_x) xs x False
  | x == 'u' = if prev == 'u'
    then 
      if first then (dir_y, dir_x, '+') : setCharDir (dir_y-1, dir_x) xs x False
      else (dir_y, dir_x, '|') :          setCharDir (dir_y-1, dir_x) xs x False
    else (dir_y, dir_x, '+') :            setCharDir (dir_y-1, dir_x) xs x False
  | x == 'l' = if prev == 'l'
    then 
      if first then (dir_y, dir_x, '+') : setCharDir (dir_y, dir_x-1) xs x False
      else (dir_y, dir_x, '-') :          setCharDir (dir_y, dir_x-1) xs x False
    else (dir_y, dir_x, '+') :            setCharDir (dir_y, dir_x-1) xs x False
  | x == 'r' = if prev == 'r'
    then 
      if first then (dir_y, dir_x, '+') : setCharDir (dir_y, dir_x+1) xs x False
      else (dir_y, dir_x, '-') :          setCharDir (dir_y, dir_x+1) xs x False
    else (dir_y, dir_x, '+') :            setCharDir (dir_y, dir_x+1) xs x False


--Z4

-- Funkce na spočítání min kolem každé volné buňky
minesInMaze :: Maze -> Maze
minesInMaze maze = [ [ processCell (x, y) | x <- [0 .. length (maze !! 0) - 1] ] | y <- [0 .. length maze - 1] ]
  where
    -- Vrátí buňku s počtem min nebo původní znak
    processCell (x, y)
      | maze !! y !! x == '*' = '*'
      | maze !! y !! x == '#' = '#'
      | otherwise = let count = countMines (x, y) in if count == 0 then '.' else head (show count)

    -- Spočítá počet min v okolí buňky
    countMines (x, y) = length [ () | dx <- [-1..1], dy <- [-1..1], isMine (x + dx, y + dy) ]
    isMine (x, y) = x >= 0 && y >= 0 && y < length maze && x < length (maze !! 0) && maze !! y !! x == '*'
