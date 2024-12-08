import Data.List (elemIndex) 
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set 
import qualified Data.Sequence as Seq

type Maze = [String]

ms :: Maze
ms =
  [ "*.......",
    ".....#..",
    "..*.*#..",
    "..*.*#..",
    ".#####..",
    "......*.",
    "........"
  ]

sample1 :: Maze
sample1 =
  [ "*********",
    "* *   * *",
    "* * * * *",
    "* * * * *",
    "*   *   *",
    "******* *",
    "        *",
    "*********"
  ]

sample2 :: Maze
sample2 =
  [ "       ",
    "       ",
    "  ***  ",
    "  ***  ",
    "  ***  ",
    "       ",
    "       "
  ]

sample3 :: Maze
sample3 = ["  * *  ",
           " ##### ",
           "  ***  ",
           "  * *  ",
           "  ***  ",
           "     * ",
           "       "]

sample4 :: Maze
sample4 = ["*********",
           "*s*   *e*",
           "* *   * *",
           "* *   * *",
           "*       *",
           "******* *",
           "        *",
           "*********"]

arrow :: Maze
arrow =
  [ "....#....",
    "...###...",
    "..#.#.#..",
    ".#..#..#.",
    "....#....",
    "....#....",
    "....#####"
  ]

-- Функция для подсчета мин вокруг ячейки
countMines :: Maze -> Int -> Int -> Int
countMines maze x y =
  length
    [1 | i <- [-1 .. 1], j <- [-1 .. 1], let nx = x + i, let ny = y + j, nx >= 0, nx < length maze, ny >= 0, ny < length (maze !! 0), maze !! nx !! ny == '*']

-- Функция для обработки всего лабиринта
minesInMaze :: Maze -> Maze
minesInMaze maze = [[updateCell x y | y <- [0 .. length (maze !! 0) - 1]] | x <- [0 .. length maze - 1]]
  where
    updateCell x y
      | cell == '*' = '*'
      | cell == '#' = '#'
      | otherwise = if mines == 0 then '.' else head (show mines)
      where
        cell = maze !! x !! y
        mines = countMines maze x y

-- Функция для печати лабиринта
printMaze :: Maze -> IO ()
printMaze = mapM_ putStrLn

--Maze above Maze
above :: Maze -> Maze -> Maze
above maze1 maze2 = concat [maze1, maze2]

--side by side
sideBySide :: Maze -> Maze -> Maze
sideBySide maze1 maze2 = zipWith (++) maze1 maze2

--Rotate Rigth
rotateR :: Maze -> Maze
rotateR maze = [[maze !! (length maze - 1 - j) !! i | j <- [0 .. length maze - 1]] | i <- [0 .. length (head maze) - 1]]

--Rotate Left
rotateL :: Maze -> Maze
rotateL maze = [[maze !! j !! (length (head maze) - 1 - i) | j <- [0 .. length maze - 1]] | i <- [0 .. length (head maze) - 1]]

--
getFromMaze :: Maze -> (Int, Int) -> Char
getFromMaze maze (row, col) = (maze !! row) !! col

--putIntoMaze
putIntoMaze :: Maze -> [(Int, Int, Char)] -> Maze
putIntoMaze maze [] = maze
putIntoMaze maze ((row, col, char) : xs) = putIntoMaze updatedMaze xs
  where
    updatedRow = take col (maze !! row) ++ [char] ++ drop (col + 1) (maze !! row)
    updatedMaze = take row maze ++ [updatedRow] ++ drop (row + 1) maze

-- Функция для извлечения части лабиринта
getPart :: Maze -> (Int, Int) -> (Int, Int) -> Maze
getPart maze (startRow, startCol) (height, width) = 
  [ take width (drop startCol (maze !! (startRow + i))) | i <- [0..height - 1]]

-- Получить стартовую позицию


-- Пример использования
main :: IO ()
main = printMaze (minesInMaze ms)