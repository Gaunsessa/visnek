module Render where

import GS
import Snake
import Util

import Graphics.Gloss
import Data.Char
import Data.Map ((!))

makeChar :: FontColor -> Char -> (Int, Int) -> Picture
makeChar col c (x, y) =
    translate (-sSize / 2) (sSize / 2) -- Translate world anchor to top left
        $ translate (fromIntegral x * (sSize / 32)) (-fromIntegral y * (sSize / 32)) -- Translates to x and y
        $ scale sFac sFac -- Scales to fit the screen res
        $ translate 4 (-4) -- Changes anchor to top left 
        $ bitmapOfBMP
        $ font ! col !! ord c
    where
        sFac  = sSize / 8 / 32
        sSize = fromIntegral screenSize

makeSnake :: Snake -> Picture
makeSnake (Snake _ _ body word compl _) =
    pictures
        $ zipWith3 
            makeChar 
            (replicate compl FGREEN ++ repeat FWHITE) 
            (word ++ repeat (toEnum 0)) 
            body

makeApples :: [(Int, Int)] -> Picture
makeApples = pictures . map (makeChar FRED '#')

makeText :: (Int, Int) -> String -> Picture
makeText p s = pictures $ zipWith (makeChar FWHITE) s [(x, snd p) | x <- [fst p..]]