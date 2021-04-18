module Main where

import Lib

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
import Graphics.Gloss
import Data.Monoid

windowPos :: (Int, Int)
windowPos = (0,0)
windowWidth :: Int
windowWidth = 600
windowHeight :: Int
windowHeight = 600
windowDimensions :: (Int, Int)
windowDimensions = (windowWidth, windowHeight)

window :: Display
window = InWindow "Sierpinsky's carpet"  windowDimensions windowPos
 
backgroundColour :: Color
backgroundColour = white
 
axes :: Picture
axes = color red (line [ (-10000, 0), (10000,  0) ]) <>
       color red (line [ (0, -10000), (0,  10000) ])
poly :: Picture
poly = polygon [ (-50,-50), ( 0, 0), ( 50,-50), ( 0,100) ]
poly2 :: Picture
poly2 = polygon [ (-50,-50), ( 0, 0), ( 50,-50), ( 0,100) ]
poly3 :: Picture
poly3 = polygon [ (-50,-50), ( 0, 0), ( 50,-50), ( 0,100) ]

minSize :: Int
minSize = 8

fillTri :: Display -> Int -> Int -> Int -> Picture
fillTri window x y size =
  translate
    (-(fromIntegral windowWidth)/2)
    (-(fromIntegral windowHeight)/2)
    (color red (polygon [(fromIntegral x, fromIntegral y), ((fromIntegral x) + (fromIntegral size), (fromIntegral y)), (fromIntegral x, (fromIntegral y) + (fromIntegral size))]))

sierpinskiTri :: Display -> Int -> Int -> Int -> Picture
sierpinskiTri w x y size =
    if size <= minSize
    then fillTri w x y size
    else let size2 = size `div` 2
      in pictures [
          sierpinskiTri w x y size2,
          (sierpinskiTri w x (y + size2) size2),
          (sierpinskiTri w (x + size2) y size2) ]

main :: IO ()
main = display window backgroundColour (sierpinskiTri window 50 50 512)