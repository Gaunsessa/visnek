module Snake where
    
import Util

import Data.List

data Snake = Snake
    {   dir   :: Direction
    ,   body  :: [(Int, Int)]
    ,   word  :: String
    ,   compl :: Int
    ,   food  :: Bool
    }

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)

tickSnake :: Snake -> Int -> Maybe Snake
tickSnake (Snake dir body word compl food) r
    | np `elem` tail body 
        || fst np < 0
        || fst np > 31
        || snd np < 0
        || snd np > 31 = Nothing
    | otherwise = Just Snake
        {   dir   = dir
        ,   body  = np : if food then body else init body
        ,   word  = if compl == length word then randword r (length body) else word
        ,   compl = if compl == length word then 0 else compl
        ,   food  = False
        }
    where 
        np = movePoint (head body) dir

eatApple :: Snake -> [(Int, Int)] -> (Snake, [(Int, Int)])
eatApple s apples = (s { food = h `elem` apples }, delete h apples)
    where h = head (body s)

changeDir :: Snake -> Direction -> Snake
changeDir s d = if oppositeDir (dir s) == d then s else s { dir = d }

movePoint :: (Int, Int) -> Direction -> (Int, Int)
movePoint (x, y) UP    = (x, y - 1)
movePoint (x, y) DOWN  = (x, y + 1)
movePoint (x, y) LEFT  = (x - 1, y)
movePoint (x, y) RIGHT = (x + 1, y)

oppositeDir :: Direction -> Direction
oppositeDir UP    = DOWN
oppositeDir DOWN  = UP
oppositeDir LEFT  = RIGHT
oppositeDir RIGHT = LEFT