module Util where

import GHC.IO
import Codec.BMP
import Data.Word
import Data.List
import Graphics.Gloss
import Data.List.Split
import Graphics.Gloss.Interface.IO.Game

import qualified Data.ByteString as B

inpwords :: [String]
inpwords = sortOn length words
    where words = map init $ lines $ unsafePerformIO $ readFile "words.txt"

bmpSection :: Rectangle -> BMP -> BMP
bmpSection (Rectangle (x, y) (w, h)) b =
    packRGBA32ToBMP w h
        $ B.pack
        $ concat
        $ reverse
        $ map (take (w * 4) . drop (x * 4)) -- Selects the width
        $ (take h . drop y) -- Selects the height
        $ reverse d -- The bmp is upside down
    where d = chunksOf (fst (bmpDimensions b) * 4) $ B.unpack $ unpackBMPToRGBA32 b

sliceBMP :: BMP -> (Int, Int) -> [BMP]
sliceBMP b (w, h) =
    [   (\(x, y) -> bmpSection (Rectangle (x * w, y * h) (w, h)) b) (x, y)
    |   y <- [0 .. ih `div` h - 1]
    ,   x <- [0 .. iw `div` w - 1]
    ]
    where (iw, ih) = bmpDimensions b

c2l :: Color -> [Word8]
c2l c = map round [r * 255, g * 255, b * 255, a * 255]
    where (r, g, b, a) = rgbaOfColor c

swapColors :: BMP -> [(Color, Color)] -> BMP
swapColors d s =
    uncurry packRGBA32ToBMP32 (bmpDimensions d)
        $ B.pack
        $ concatMap
            (\c -> foldl (\d s -> if d == c2l (fst s) then c2l (snd s) else d) c s)
            (chunksOf 4 $ B.unpack $ unpackBMPToRGBA32 d)

randword :: Int -> Int -> String
randword r l = 
    if length word /= l 
        then randword ((1103515245 * r + 12345) `mod` 2147483648) (l - length word - 1) ++ " " ++ word 
        else word
    where 
        words = filter (\x -> length x == l || length x <= l - 2) inpwords
        word = words !! (r `mod` length words)

keyToChar :: Key -> Char
keyToChar (Char k) = k
keyToChar (SpecialKey KeySpace) = ' '
keyToChar _ = toEnum 0