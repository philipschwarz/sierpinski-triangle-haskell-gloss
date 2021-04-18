module Main where

import Lib

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
import Graphics.Gloss
import Data.Monoid

title = "Sierpinsky's carpet"
windowPosition = (0,0)
width = 600
height = 600
dimensions = (width, height)
backgroundColour = white

carpetSize = 512
carpetXPos = 50
carpetYPos = 50

horizontalShift = -(fromIntegral width)/2
verticalShift = -(fromIntegral height)/2

window :: Display
window = InWindow title dimensions windowPosition

minSize :: Int
minSize = 8

fillTriangle :: Int -> Int -> Int -> Picture
fillTriangle x y size = 
  let 
    xPos = fromIntegral x
    yPos = fromIntegral y
    side = fromIntegral size
    p1 = (xPos, yPos)
    p2 = (xPos + side, yPos)
    p3 = (xPos, yPos + side)
    triangle = color red (polygon [p1, p2, p3])
  in translate horizontalShift verticalShift triangle

sierpinskiCarpet :: Int -> Int -> Int -> Picture
sierpinskiCarpet x y size =
  if size <= minSize
  then fillTriangle x y size
  else 
    let size2 = size `div` 2
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