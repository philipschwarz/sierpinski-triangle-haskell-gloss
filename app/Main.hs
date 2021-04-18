module Main where

import Lib

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
import Graphics.Gloss
import Data.Monoid

title :: [Char]
title = "Sierpinsky's carpet"
position :: (Int, Int)
position = (0,0)
width :: Int
width = 600
height :: Int
height = 600
dimensions :: (Int, Int)
dimensions = (width, height)

window :: Display
window = InWindow title dimensions position

minSize :: Int
minSize = 8

fillTriangle :: Int -> Int -> Int -> Picture
fillTriangle x y size =
  translate
    (-(fromIntegral width)/2)
    (-(fromIntegral height)/2)
    (color red (polygon [(fromIntegral x, fromIntegral y), ((fromIntegral x) + (fromIntegral size), (fromIntegral y)), (fromIntegral x, (fromIntegral y) + (fromIntegral size))]))

sierpinskiTriangle :: Int -> Int -> Int -> Picture
sierpinskiTriangle x y size =
    if size <= minSize
    then fillTriangle x y size
    else let size2 = size `div` 2
      in pictures [
        sierpinskiTriangle x y size2,
        sierpinskiTriangle x (y + size2) size2,
        sierpinskiTriangle (x + size2) y size2 ]

main :: IO ()
main =
  display
    window
    white
    (sierpinskiTriangle 50 50 512)