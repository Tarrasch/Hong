-- | The module defining how to paint a state
module PaintGame
  (statePic)
  where

import GameState
import HongConstants
import Control.Monad
import Fal.Picture
import Fal.Region
import Fal.Shape
import Graphics.SOE.Gtk (Window)

----------------- Quickstart -----------------

-- Test running this function to see a static image of the game

test ::  IO ()
test = draw "Does it look good?" (statePic startState)

----------------- Functions -----------------

-- | Given a state of the game, paint the ball, borders and paddles.
statePic :: State -> Picture
statePic s = foldl Over EmptyPic
   [ ballPic        (ballPos s)
   , leftPaddlePic  (leftPaddle  s)
   , rightPaddlePic (rightPaddle s)
   , bordersPic ]

ballPic :: Vector -> Picture
ballPic v = Region ballColor reg
  where reg = Translate v $ Shape $ circle ballRadius

paddlePic :: Vector -> Picture
paddlePic v = Region paddleColor reg
  where reg = Translate v $ Shape $ Rectangle paddleWidth (2*paddleHalfHeight)

leftPaddlePic, rightPaddlePic :: Float -> Picture
leftPaddlePic  y = paddlePic (-planeHalfWidth - paddleWidth/2 - ballRadius, y)
rightPaddlePic y = paddlePic ( planeHalfWidth + paddleWidth/2 + ballRadius, y)

bordersPic :: Picture
bordersPic = Region borderColor reg
  where reg   = roof `Union` floor
        roof  = Translate (0,  planeHalfHeight + borderHeight/2 + ballRadius) $ Shape shp
        floor = Translate (0, -planeHalfHeight - borderHeight/2 - ballRadius) $ Shape shp
        shp   = Rectangle (2*planeHalfWidth+2*ballRadius) borderHeight
