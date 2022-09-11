module Main where

-- I use unsafePerformIO in inpwords and font because I cant get embedFile to work

import GS
import Render
import Snake
import Util

import System.Random
import System.Directory
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import Data.Maybe

randapple :: [(Int, Int)] -> IO (Int, Int)
randapple a = do
    p1 <- randomIO
    p2 <- randomIO

    let na = (p1 `mod` 32, p2 `mod` 32)

    if length a == 32 * 32 then return na else 
        if na `elem` a then randapple a else return na

handleRender :: GS -> IO Picture
handleRender gs
    | state gs == CONTROL || state gs == INSERT = 
        return $ pictures
            [   makeApples (apples gs)
            ,   makeSnake (snake gs)
            ]
    | state gs == MENU = 
        return $ pictures
            [   makeText (0, 0) "O------------------------------O"
            ,   pictures 
                    [ makeText (0, y) ("|" ++ replicate 30 (toEnum 1) ++ "|") | y <- [1..30] ]
            ,   makeText (0, 31) "O------------------------------O"
            ,   scale 1.5 1.5 $ pictures
                    [   makeText (13, 10) "VISNEK!"
                    ,   makeText (12, 13) "HIGHSCORE"
                    ,   makeText (16, 15) $ show $ highscore gs
                    ,   makeText (11, 20) $ "PRESS" ++ [toEnum 1] ++ "ENTER"
                    ]
            ]
    | otherwise = return blank

handleEvent :: Event -> GS -> IO GS
handleEvent (EventKey key Down _ _) gs
    | state gs == MENU = return $ case key of
        SpecialKey KeyEnter -> gs { state = CONTROL }
        _ -> gs
    | state gs == CONTROL = return $ case key of
        Char 'k' -> gs { snake = changeDir psnake UP }
        Char 'j' -> gs { snake = changeDir psnake DOWN }
        Char 'h' -> gs { snake = changeDir psnake LEFT }
        Char 'l' -> gs { snake = changeDir psnake RIGHT }
        Char 'i' -> gs { state = INSERT }
        _ -> gs
    | state gs == INSERT = return $ handleEventInsert key gs
    where psnake = snake gs
handleEvent _ gs = return gs

handleEventInsert :: Key -> GS -> GS
handleEventInsert (SpecialKey KeyEsc) gs = gs { state = CONTROL }
handleEventInsert k gs
    | pcompl == length (body psnake) = gs
    | word psnake !! pcompl == keyToChar k = gs { snake = psnake { compl = pcompl + 1 } }
    | pcompl > 0 = gs { snake = psnake { compl = pcompl - 1 } }
    | otherwise = gs
    where 
        psnake = snake gs
        pcompl = compl psnake

handleTick :: Float -> GS -> IO GS
handleTick d gs 
    | state gs == MENU = return gs
    | ct gs < tpt gs = return gs { ct = ct gs + d }
    | otherwise =  do
        r <- randomIO

        case tickSnake (snake gs) r of
            Nothing -> do
                when (length (body $ snake gs) > highscore gs) 
                    $ writeFile highscoreFile $ show $ length (body $ snake gs)

                return GS
                    {   state     = MENU
                    ,   highscore = if length (body $ snake gs) > highscore gs 
                            then length (body $ snake gs) 
                            else highscore gs
                    ,   snake = Snake
                            {   dir   = UP
                            ,   moves = []
                            ,   body  = [(15, 13 + y) | y <- [0..3]]
                            ,   word  = randword r 4
                            ,   compl = 0
                            ,   food  = False
                            }
                    ,   apples = []
                    ,   tpt    = 0.5
                    ,   ct     = 0
                    }
            Just ns1 -> do
                let (ns2, na) = eatApple ns1 (apples gs)

                nap <- randapple (apples gs)

                return gs 
                    {   snake = ns2
                    ,   apples = if length (word ns2) == compl (snake gs) then nap : na else na
                    ,   tpt = if length na < length (apples gs)
                            then if tpt gs - 0.02 < 0.1 then 0.1 else tpt gs - 0.02
                            else tpt gs
                    ,   ct = 0
                    }

main :: IO ()
main = do
    rnd <- randomIO

    hf <- do 
        fe <- doesFileExist highscoreFile

        if fe
            then readFile highscoreFile
            else do
                writeFile highscoreFile "0"
                return "0"

    -- This makes the filter of inpwords happen at the start
    putStrLn $ head inpwords

    playIO
        (InWindow "visnek" (screenSize, screenSize) (0, 0))
        (makeColorI 1 40 36 255)
        60
        GS
            {   state     = MENU
            ,   highscore = read hf
            ,   snake     = Snake
                    {   dir   = UP
                    ,   moves = []
                    ,   body  = [(15, 13 + y) | y <- [0..3]]
                    ,   word  = randword rnd 4
                    ,   compl = 0
                    ,   food  = False
                    }
            ,   apples = []
            ,   tpt    = 0.5
            ,   ct     = 0
            }
        handleRender
        handleEvent
        handleTick