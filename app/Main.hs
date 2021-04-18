module Main where

import Lib

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
import Graphics.Gloss
import Data.Monoid

title :: [Char]
title = "Sierpinsky's carpet"
windowPosition :: (Int, Int)
windowPosition = (0,0)
width :: Int
width = 600
height :: Int
height = 600
dimensions :: (Int, Int)
dimensions = (width, height)
backgroundColour :: Color
backgroundColour = white

carpetSize :: Int
carpetSize = 512
carpetXPos :: Int
carpetXPos = 50
carpetYPos :: Int
carpetYPos = 50

horizontalShift = -(fromIntegral width)/2
verticalShift = -(fromIntegral height)/2

window :: Display
window = InWindow title dimensions windowPosition

minSize :: Int
minSize = 8

fillTriangle :: Int -> Int -> Int -> Picture
fillTriangle x y size =
  let triangle = (color red (polygon [(fromIntegral x, fromIntegral y), ((fromIntegral x) + (fromIntegral size), (fromIntegral y)), (fromIntegral x, (fromIntegral y) + (fromIntegral size))]))
  in translate horizontalShift verticalShift triangle

sierpinskiCarpet :: Int -> Int -> Int -> Picture
sierpinskiCarpet x y size =
    if size <= minSize
    then fillTriangle x y size
    else let size2 = size `div` 2
      in pictures [
        sierpinskiCarpet x y size2,
        sierpinskiCarpet x (y + size2) size2,
        sierpinskiCarpet (x + size2) y size2 ]

main :: IO ()
main =
  display
    window
    backgroundColour
    (sierpinskiCarpet carpetXPos carpetYPos carpetSize)