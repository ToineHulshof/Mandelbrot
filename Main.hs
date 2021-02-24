module Main where

import Data.Binary (encode)
import Data.Int (Int8)
import Data.Foldable (fold)
import qualified Data.ByteString.Lazy.Char8 as B

type RGB = (Int8, Int8, Int8)
type HSV = (Double, Double, Double)
type Complex = (Double, Double)

width :: Int
width = 2560

height :: Int 
height = 1600

maxDepth :: Int
maxDepth = 200

byteRGB :: RGB -> [B.ByteString]
byteRGB (r, g, b) = [encode r, encode g, encode b]

coordinateToComplex :: (Int, Int) -> Complex
coordinateToComplex (y, x) = (fromIntegral x / scale - 1 - 2 * offset, fromIntegral y / scale - 1)
    where 
        scale = fromIntegral height / 2
        offset = (fromIntegral (width - height) / fromIntegral height) / 2

escapeTime :: Complex -> (Int, Complex)
escapeTime z = (escape z (-0.8, 0.156) maxDepth, z)
-- escapeTime z = (escape z z maxDepth, z)

depthTohsv :: (Int, Complex) -> HSV
depthTohsv (x, z) = (360 - (fromIntegral x * 120) / fromIntegral maxDepth, 100, if x == 0 then 0 else 110 - (fromIntegral x * 100 / fromIntegral maxDepth))
-- depthTohsv (x, z) = (360 - ((fromIntegral x + 1 - (log (norm z))) * 360) / fromIntegral maxDepth, 100, if x == 0 then 0 else 100)
-- depthTohsv x = (x * 240 `div` maxDepth, fromIntegral x * 100 / fromIntegral maxDepth, fromIntegral x * 80 / fromIntegral maxDepth)

hsvTorgb :: HSV -> RGB
hsvTorgb (h, s, v) = (round $ (r' + m) * 255, round $ (g' + m) * 255, round $ (b' + m) * 255)
    where
        (r', g', b') = hueTorgb (h, c, x)
        c = v / 100 * s / 100
        x = c * (1 - abs ((h / 60) % 2 - 1))
        m = v / 100 - c

(%) :: Double -> Double -> Double
(%) x y
    | x < 2 = x
    | otherwise = (x - y) % y

hueTorgb :: HSV -> HSV
hueTorgb (h, c, x)
    | h < 60 = (c, x, 0)
    | h < 120 = (x, c, 0)
    | h < 180 = (0, c, x)
    | h < 240 = (0, x, c)
    | h < 300 = (x, 0, c)
    | otherwise = (c, 0, x)

pixels :: [RGB]
pixels = map (hsvTorgb . depthTohsv . escapeTime . coordinateToComplex) $ (,) <$> [0 .. height - 1] <*> [0 .. width - 1]

norm :: Complex -> Double
norm (r, i) = sqrt (r ^ 2 + i ^ 2)

square :: Complex -> Complex
square (a, b) = (a ^ 2 - b ^ 2, 2 * a * b)

add :: Complex -> Complex -> Complex
add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

escape :: Complex -> Complex -> Int -> Int
escape z c d
    | norm z > 2 || d == 0 = d
    | otherwise = escape (square z `add` c) c (d - 1)

main :: IO ()
main = B.writeFile "mandelbrot.ppm" $ B.pack ("P6\n" ++ show width ++ " " ++ show height ++ "\n255\n") `B.append` fold (concatMap byteRGB pixels)