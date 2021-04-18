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
triangleColour = red

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
    bottomLeftPoint  = (xPos       , yPos)
    bottomRightPoint = (xPos + side, yPos)
    topPoint         = (xPos       , yPos + side)
    triangle = polygon [bottomLeftPoint, bottomRightPoint, topPoint]
    colouredTriangle = color triangleColour triangle
  in translate horizontalShift verticalShift colouredTriangle

sierpinskiCarpet :: Int -> Int -> Int -> Picture
sierpinskiCarpet x y size =
  if size <= minSize
  then fillTriangle x y size
  else
    let size2 = size `div` 2
    in pictures [ sierpinskiCarpet x y size2,
                  sierpinskiCarpet x (y + size2) size2,
                  sierpinskiCarpet (x + size2) y size2 ]

main :: IO ()
main =
  let carpet = sierpinskiCarpet carpetXPos carpetYPos carpetSize
  in display window backgroundColour carpet