module GS where

import Util
import Snake

import GHC.IO
import Codec.BMP
import Data.Either
import Graphics.Gloss

import qualified Data.Map as Map

data GS = GS
    {   state     :: State
    ,   highscore :: Int
    ,   snake     :: Snake
    ,   apples    :: [(Int, Int)]
    ,   tpt       :: Float
    ,   ct        :: Float
    }

data State = MENU | CONTROL | INSERT deriving (Show, Eq)

data FontColor = FWHITE | FRED | FGREEN | FBLACK deriving (Show, Eq, Ord, Enum)

font :: Map.Map FontColor [BMP]
font = 
    Map.fromList 
        [ (fst c, map (`swapColors` [(black, snd c)]) base) 
        | c <- zip 
            [FWHITE ..] 
            [   makeColorI 252 222 234 255
            ,   makeColorI 255 77 110 255
            ,   makeColorI 38 89 53 255
            ,   makeColorI 1 40 36 255
            ] 
        ]
    where base = 
            sliceBMP 
                (swapColors 
                    (head $ rights [unsafePerformIO (readBMP "font.bmp")]) 
                    [(white, makeColorI 0 0 0 0)]) 
                (8, 8)

screenSize :: Int
screenSize = 8 * 32 * 3

highscoreFile :: String
highscoreFile = "highscore.txt"