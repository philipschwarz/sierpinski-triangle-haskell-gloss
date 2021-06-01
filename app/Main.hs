module Main where

import Lib

import Graphics.Gloss

title = "Sierpinski"
windowPosition = (0,0)
width = 600
height = 600
dimensions = (width, height)
backgroundColour = white
triangleColour = red

triangleSize = 512
triangleXPos = 50
triangleYPos = 50

horizontalShift = -(fromIntegral width)/2
verticalShift = -(fromIntegral height)/2

windowDisplay :: Display
windowDisplay = InWindow title dimensions windowPosition

minSize :: Int
minSize = 4

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
  in colouredTriangle

sierpinskiTriangle :: Int -> Int -> Int -> Picture
sierpinskiTriangle x y size =
  if size <= minSize
  then fillTriangle x y size
  else let halfSize = size `div` 2
       in pictures [ sierpinskiTriangle x y halfSize,
                     sierpinskiTriangle x (y + halfSize) halfSize,
                     sierpinskiTriangle (x + halfSize) y halfSize ]

main :: IO ()
main =
  let
    triangle = sierpinskiTriangle triangleXPos triangleYPos triangleSize
    shiftedTriangle = (translate horizontalShift verticalShift triangle)
  in display windowDisplay backgroundColour shiftedTriangle    